// State scale analysis of RICE pvpm model
// MOD9A1 data has been loaded before as modTerra
// Images for the study site arkansasRice2020

// Rice Fields of Arkansas


// ARKANSAS SHAPEFILE //
var states = ee.FeatureCollection('TIGER/2018/States'); // Load the US States dataset
var arkansasBoundary = states.filter(ee.Filter.eq('STUSPS', 'AR')); // Filter the dataset to obtain the boundary of Arkansas
Map.addLayer(arkansasBoundary, {color: 'red'}, 'Arkansas Boundary'); // Display the boundary on the map

// MODIS IMAGES //
var MOD09A1Collection = ee.ImageCollection("MODIS/006/MOD09A1")
.filterDate("2020-01-01", "2020-12-31").filterBounds(arkansasBoundary);//Growing season of 2020

var clipImage = function(image) {
  return image.clip(arkansasBoundary);
}; // Function to clip an image

// Apply clipImage function to each image in the MODIS ImageCollection
var MOD09A1Collection = MOD09A1Collection.map(clipImage);
// Function to extract bitwise
function bitwiseExtract(value, fromBit, toBit) {
  if (toBit === undefined) toBit = fromBit;
  var maskSize = ee.Number(1).add(toBit).subtract(fromBit);
  var mask = ee.Number(1).leftShift(maskSize).subtract(1);
  return value.rightShift(fromBit).bitwiseAnd(mask);
}

// Masking function
// Function to filter the images having cloud, cloud shadow, aerosol;
var maskMOD09A1Clouds = function (image) {
  var qa = image.select('StateQA');
  var cloudState = bitwiseExtract(qa, 0, 1); 
  // var cloudShadowState = bitwiseExtract(qa, 2);
  // var cirrusState = bitwiseExtract(qa, 8, 9);
  var aerosolQuantity = bitwiseExtract(qa, 6, 7); 
  var mask = cloudState.eq(0) // Clear
    // .and(cloudShadowState.eq(0)) // No cloud shadow
    // .and(cirrusState.eq(0)) // No cirrus
    .and(aerosolQuantity.lte(1)); // No aerosol quantity
  var maskedImage = image.updateMask(mask);
  return maskedImage; 
};

// filter and cloud-mask image collection 
var maskmod = MOD09A1Collection.map(maskMOD09A1Clouds);

// scaling converting the modis band by the required scaling
var addscaleb = function(image) {
  var scaleb1= image.expression('float(b("sur_refl_b01")/10000)').rename('scaleb1');
  var scaleb2= image.expression('float(b("sur_refl_b02")/10000)').rename('scaleb2');
  var scaleb3= image.expression('float(b("sur_refl_b03")/10000)').rename('scaleb3');
  var scaleb4= image.expression('float(b("sur_refl_b03")/10000)').rename('scaleb4');
  return image.addBands(scaleb1).addBands(scaleb2).addBands(scaleb3).addBands(scaleb4)
};

var modisb1 = maskmod.map(addscaleb);
// print('modisb1', modisb1)
var modisb3 = modisb1.select(['sur_refl_b01','sur_refl_b02','sur_refl_b03','sur_refl_b04','scaleb1','scaleb2','scaleb3','scaleb4','sur_refl_b06'], ['sur_refl_b01','sur_refl_b02','sur_refl_b03','sur_refl_b04','scaleb1','scaleb2','scaleb3','scaleb4','sur_refl_b06'])
    
// NDVI Formula 
var addBands = function(image) {
  var ndvi = image.normalizedDifference(['sur_refl_b02', 'sur_refl_b01']).rename('NDVI');
  var evi= image.expression(
    '2.5 * ((NIR - RED) / (NIR + 6 * RED - 7.5 * BLUE + 1))', {
      'RED': image.select("scaleb1"),
      'NIR': image.select("scaleb2"),
      'BLUE': image.select('scaleb3')}).rename('EVI');
  var lswi = image.normalizedDifference(['sur_refl_b02', 'sur_refl_b06']).rename('LSWI');
  return image.addBands(ndvi).addBands(evi).addBands(lswi)
};

var MODISimageswithbands = modisb3.map(addBands);
// print("MODISimageswithbands", MODISimageswithbands)
var MODISimageswithbands = MODISimageswithbands.select(['EVI','NDVI','LSWI'], ['EVI','NDVI','LSWI']) //select the required bands
 


// Gapfill the images EVI and LSWI
var mod_evi = MODISimageswithbands.select(['EVI'])
// print(mod_evi)

// EVI gapfill//
//fill the gaps in the original MODIS NDVI time series by linear interpolation
var interl_m = require('users/Yang_Chen/GF-SG:Interpolation_v1'); //using yang package
var frame  = 8*7; 
var nodata = -9999; 
var mod_ndvi_interp = interl_m.linearInterp(mod_evi, frame, nodata);
print(mod_ndvi_interp, "mod_ndvi_interp")
// print("mod_ndvi_interp", mod_ndvi_interp)
var mod_ndvi_interp0 = mod_ndvi_interp.select(['MOD_NDVI_INTER']);

// print("mod_ndvi_interp0", mod_ndvi_interp0)

//Smooth the MODIS interpolation time series by the Savitzky–Golay filter
var sg_filter = require('users/Yang_Chen/GF-SG:SG_filter_v1');
//Reduce the residual noise in the synthesized NDVI time series by the weighted SG filter
var list_trend_sgCoeff = ee.List([-0.070588261,-0.011764720,0.038009040,0.078733027,0.11040724,0.13303168,0.14660634,
0.15113123,0.14660634,0.13303168,0.11040724,0.078733027,0.038009040,-0.011764720,-0.070588261]);   //suggested trend parameter:(7,7,0,2)
var list_sg_coeff = ee.List([0.034965038,-0.12820521,0.069930017,0.31468537,0.41724950,0.31468537,
0.069930017,-0.12820521,0.034965038]);   //suggested parameters of SG:(4,4,0,5)
var syn_series_sg = sg_filter.sg_filter_chen(mod_ndvi_interp0,list_trend_sgCoeff,list_sg_coeff);
print(syn_series_sg, "syn_series_sg");

function renameBandsETM(image) {
    var bands = ['MOD_NDVI_SG'];
    var new_bands = ['EVI_SG'];
    return image.select(bands).rename(new_bands);
}
var syn_series_sg_evirenamed = syn_series_sg
  .map(renameBandsETM)

var mod1 = MODISimageswithbands;
var mod2 = syn_series_sg_evirenamed.select('EVI_SG');
// Use an equals filter to define how the collections match.
var filter = ee.Filter.equals({
  leftField: 'system:index',
  rightField: 'system:index'
});

// Create the join.
var simpleJoin = ee.Join.simple();

// Applt join
var mod1join = ee.ImageCollection(simpleJoin.apply(mod1, mod2, filter));
var mod2join = ee.ImageCollection(simpleJoin.apply(mod2, mod1, filter));

// print('Joined', mod1join, mod2join)

var final_col = mod1join.map(function(img){
  // Create a collection with 1 image
  var temp = ee.ImageCollection(ee.List([img]));
  // Apply join to collection 2
  // Resulting collection will have 1 image with exact same date as img
  var join = simpleJoin.apply(mod2join, temp, filter);
  // Get resulting image
  var i2 = ee.Image(join.first())
  return img.addBands(i2)
})
print('final_col', final_col)



// LSWI gapfill//
var mod_lswi = MODISimageswithbands.select(['LSWI'])
var frame  = 8*7; 
var nodata = -9999; 
var mod_ndvi_interp2 = interl_m.linearInterp(mod_lswi, frame, nodata);
print(mod_ndvi_interp2, "mod_ndvi_interp")
// print("mod_ndvi_interp", mod_ndvi_interp)
var mod_ndvi_interp0 = mod_ndvi_interp2.select(['MOD_NDVI_INTER']);
// print("mod_ndvi_interp0", mod_ndvi_interp0)
print(mod_ndvi_interp0, "mod_ndvi_interp0_LSWI")

function renameBandsETMLSWI(image) { //rename the LSWI band
    var bands = ['MOD_NDVI_INTER'];
    var new_bands = ['LSWI_INP'];
    return image.select(bands).rename(new_bands);
}
var syn_series_inp_lswirenamed = mod_ndvi_interp0
  .map(renameBandsETMLSWI)

// var mod1 = final_col;
// var mod2 = syn_series_inp_lswirenamed.select('LSWI_INP');
// // Use an equals filter to define how the collections match.
// var filter = ee.Filter.equals({
//   leftField: 'system:index',
//   rightField: 'system:index'
// });
// // Create the join.
// var simpleJoin = ee.Join.simple();
// // Apply join
// var mod1join = ee.ImageCollection(simpleJoin.apply(mod1, mod2, filter));
// var mod2join = ee.ImageCollection(simpleJoin.apply(mod2, mod1, filter));
// // print('Joined', mod1join, mod2join)
// var final_col = mod1join.map(function(img){
//   // Create a collection with 1 image
//   var temp = ee.ImageCollection(ee.List([img]));
//   // Apply join to collection 2
//   // Resulting collection will have 1 image with exact same date as img
//   var join = simpleJoin.apply(mod2join, temp, filter);
//   // Get resulting image
//   var i2 = ee.Image(join.first())
//   return img.addBands(i2)
// })
// print('final_col', final_col)


var mod1 = final_col
var mod2 = syn_series_inp_lswirenamed
var filter = ee.Filter.equals({
  leftField: 'system:time_start',
  rightField: 'system:time_start'
});
// Create the join.
var simpleJoin = ee.Join.inner();
// Inner join
var innerJoin = ee.ImageCollection(simpleJoin.apply(mod1, mod2, filter))
var final_col = innerJoin.map(function(feature) {
  return ee.Image.cat(feature.get('primary'), feature.get('secondary'));
})

print('final_col', final_col)



// /////////////////NCEP Dataset// /////////////////
var startDate = ee.Date('2020-01-01')
var endDate = ee.Date('2020-12-31')
var collection = ee.ImageCollection("NOAA/CFSV2/FOR6H")
var collection = collection.filterDate('2020-01-01', '2020-12-31')

// Renaming the bands
function renameBandsETMdswr(image) {
    var bands = ['Downward_Short-Wave_Radiation_Flux_surface_6_Hour_Average', 'Temperature_height_above_ground', 'Maximum_temperature_height_above_ground_6_Hour_Interval'];
    var new_bands = ['DSWR', 'Temp', 'Tmax'];
    return image.select(bands).rename(new_bands);
}
var collection = collection.map(renameBandsETMdswr)

// 6 hourly to daily
var numberOfDays = endDate.difference(startDate, 'days')
var daily1 = ee.ImageCollection(
  ee.List.sequence(0, numberOfDays.subtract(1))
    .map(function (dayOffset) {
      var start = startDate.advance(dayOffset, 'days')
      var end = start.advance(1, 'days')
      return collection
        .filterDate(start, end)
        .mean()
        .set('system:time_start', start.millis());
    })
);

var eightday = daily1.filterDate('2020-01-01', "2020-12-31");
var startDate = ee.Date('2020-01-01');
var endDate = ee.Date('2020-12-31');
var dayOffsets = ee.List.sequence(
  0, 
  endDate.difference(startDate, 'days').subtract(1),
  8 // Single day every week
)

var weeklyMeans = ee.ImageCollection.fromImages(
  dayOffsets.map(function(dayOffset) {
    var start = startDate.advance(dayOffset, 'day')
    var end = start.advance(8, 'day')
    return eightday
      .filterDate(start, end)
      .mean()
      .set('system:time_start', start.millis());
  })  
);

var TempSWR = weeklyMeans.select(['Tmax', 'Temp', 'DSWR']);

///Combine two image collections
var mod1 = final_col
var mod2 = TempSWR
var filter = ee.Filter.equals({
  leftField: 'system:time_start',
  rightField: 'system:time_start'
});
// Create the join.
var simpleJoin = ee.Join.inner();
// Inner join
var innerJoin = ee.ImageCollection(simpleJoin.apply(mod1, mod2, filter))
var joined = innerJoin.map(function(feature) {
  return ee.Image.cat(feature.get('primary'), feature.get('secondary'));
})

print('Joined', joined)

// DOP DOH Calcuation [Line 224 -]
// Rearranging the EVI data
var bandSubset = joined.select(['EVI_SG'], ['EVI_SG']) // PVPM calculation starts here
// add doy
var addDate = function(image){
  var doy = image.date().getRelative('day', 'year');
  var doyBand = ee.Image.constant(doy).uint16().rename('doy')
  return image.addBands(doyBand);
};

var floatcollection = function(image){
  image.float();
  return image;
};

var bandSubset = bandSubset.map(addDate)
var bandSubset = bandSubset.map(floatcollection)
print(bandSubset, "bandSubset") // 3 joins run well

// Define time range
// FULL TS
var startyear = 2020;
var endyear = 2020;
var startmonth = 1  
var endmonth = 12
var startday = 1
var endday = 31

var startdate = ee.Date.fromYMD(startyear,startmonth,startday);
var enddate = ee.Date.fromYMD(endyear,endmonth,endday);

// create list for years
var years = ee.List.sequence(startyear,endyear);
// create list for months
var months = ee.List.sequence(1,12);
var maxevi = bandSubset.reduce(ee.Reducer.max(bandSubset.first().bandNames().size()))
print(maxevi, "maxevi")
//print(bandSubset, "bandSubset")

var max = ee.ImageCollection(
    years.map(function (y) {
      var start = ee.Date.fromYMD(y, startmonth,startday);
      var stop = ee.Date.fromYMD (y,endmonth,endday);
      var x = bandSubset.filterDate(start, stop)
      var w = x.qualityMosaic('EVI_SG').select('doy', 'EVI_SG')
    return w.set({'year': y})
}).flatten());
print(max,'max')
//Map.addLayer(max, {min: 1, max: 365}, 'max')


var bandSubset = bandSubset.select(['EVI_SG', "doy"], ['EVI_SG', "doy"])


// Before
// Find the day of min before day of max for each year
var min_before = ee.ImageCollection(
    years.map(function (y) {
      var start = ee.Date.fromYMD(y, startmonth,startday);
      var stop = ee.Date.fromYMD (y,endmonth,endday);
      var x = bandSubset.filterDate(start, stop)
      var z = ee.Image(max
        .filterMetadata('year', 'equals', y).first());
      var w = x.map(
        function(img) {
        var date = img.select('doy')
        var k = img.updateMask(z.gt(date))
      return k
}).select('EVI_SG', 'doy').reduce(ee.Reducer.min(2)).rename('EVI_SG_min_before','doy')
  return w
}).flatten());
print(min_before,'min_before')
//Map.addLayer(min_before, {min: 1, max: 365}, 'min_before')

// After
// Find the day of min after of max for each year
var min_after = ee.ImageCollection(
    years.map(function (y) {
      var start = ee.Date.fromYMD(y, startmonth,startday);
      var stop = ee.Date.fromYMD (y,endmonth,endday);
      var x = bandSubset.filterDate(start, stop)
      var z = ee.Image(max
        .filterMetadata('year', 'equals', y).first());
      var w = x.map(
        function(img) {
        var date = img.select('doy')
        var k = img.updateMask(z.lt(date))
      return k
}).select('EVI_SG', 'doy').reduce(ee.Reducer.min(2)).rename('EVI_SG_min_after','doy')
  return w
}).flatten());
print(min_after,'min_after')
//Map.addLayer(min_after, {min: 1, max: 365}, 'min_after')


// add the layer of the shapefile and rasterfile
// Map.addLayer(arkansasRice2020)
// 

var bandSubsetonlyEVI = bandSubset.select(['EVI_SG'], ['EVI_SG'])
//Map.addLayer(bandSubsetonlyEVI, null, 'bandSubsetonlyEVI')

// renaming the max collection evi to max evi
var max = max.select(['EVI_SG', 'doy'],['EVI_SG_max', "doy_max"])
//print(max, "max")
//print(eviwithdop, "eviwithdop")

// converting the collection to image
var maximg = ee.Image(max.first());
//print("maximg", maximg)
//

//  add max minbefore and min after with evidop
var adddmaxminpoints = function(image) {
  var maxminpoints = maximg.select("EVI_SG_max", "doy_max");
  return image.addBands(maxminpoints);
};

var eviwithdopmaxmin = joined.map(adddmaxminpoints);
print("eviwithdopmaxmin", eviwithdopmaxmin)

//Map.addLayer(eviwithdopmaxmin, null, 'eviwithdopmaxmin')


// min before
// converting the collection to image
var min_beforeimg = ee.Image(min_before.first());
//print("min_beforeimg", min_beforeimg)



//  add max minbefore and min after with evidop
var adddminbeforepoints = function(image) {
  var minbeforepoints = min_beforeimg.select("EVI_SG_min_before");
  return image.addBands(minbeforepoints);
};

var eviwithdopmaxmin = eviwithdopmaxmin.map(adddminbeforepoints);
//print("eviwithdopmaxmin", eviwithdopmaxmin)

//Map.addLayer(eviwithdopmaxmin, null, 'eviwithdopmaxmin')

// min after
// converting the collection to image
var min_afterimg = ee.Image(min_after.first());
//print("min_afterimg", min_afterimg)



//  add max minbefore and min after with evidop
var adddmin_afterpoints = function(image) {
  var minafterpoints = min_afterimg.select("EVI_SG_min_after");
  return image.addBands(minafterpoints);
};

var eviwithdopmaxmin = eviwithdopmaxmin.map(adddmin_afterpoints);
//print("eviwithdopmaxmin", eviwithdopmaxmin)

//Map.addLayer(eviwithdopmaxmin, null, 'eviwithdopmaxmin')







var eviwithdopmaxminimg = ee.Image(eviwithdopmaxmin.first());

var addthresholdparams = function(image) {
  var evipoints = eviwithdopmaxminimg.select("EVI_SG_max", "EVI_SG_min_before", "EVI_SG_min_after", "doy_max");
  return image.addBands(evipoints);
};

var eviwithdop = bandSubset.map(addthresholdparams);
// print(eviwithdop, "eviwithdop")
// Map.addLayer(eviwithdop, null, "eviwithdop")



// calculate g1 and g2
var calculateg1 = function(image) {
  var g1band= image.expression(
    '(EVI_SG_max)-(EVI_SG_min_before)', {
      'EVI_SG_max': image.select("EVI_SG_max"),
      'EVI_SG_min_before': image.select("EVI_SG_min_before")}).rename('g1');
  return image.addBands(g1band)
};
var eviwithdop = eviwithdop.map(calculateg1);
// print(eviwithdop)

var calculateg2 = function(image) {
  var g2band= image.expression(
    '(EVI_SG_max)-(EVI_SG_min_after)', {
      'EVI_SG_max': image.select("EVI_SG_max"),
      'EVI_SG_min_after': image.select("EVI_SG_min_after")}).rename('g2');
  return image.addBands(g2band)
};
var eviwithdop = eviwithdop.map(calculateg2);
//print(eviwithdop)
//Map.addLayer(eviwithdop, null, "eviwithdop")


// Calculate the DOP with all the data not only by training set
var calculateDOP = function(image) {
  var DOP= image.expression(
    '-60.84750+ (EVI_SG_min_after  *121.67564)+ (61.21313  * g1) + (0.60133  * doy_max)', {
      'EVI_SG_min_after': image.select("EVI_SG_min_after"),
      'g1': image.select("g1"),
      'doy_max': image.select("doy_max"),
      
    }).rename('DOP');
  return image.addBands(DOP)
};
var eviwithdop = eviwithdop.map(calculateDOP);

// Calculate the DOH all the data not only by training set
var calculateDOH = function(image) {
  var DOH= image.expression(
    '30.46275  + (EVI_SG_min_after* 139.24409) +( 73.85717 *g1) + (0.75480 * doy_max)', {
      'EVI_SG_min_after': image.select("EVI_SG_min_after"),
      'g1': image.select("g1"),
      'doy_max': image.select("doy_max"),
      
    }).rename('DOH');
  return image.addBands(DOH)
};
var eviwithdop = eviwithdop.map(calculateDOH);
//Map.addLayer(eviwithdop, null, "eviwithdop")


// need to reduce the eviwithdop image collection to an image
// Reduce the collection.
var eviwithdopmean = eviwithdop.reduce(ee.Reducer.mean());
//Map.addLayer(eviwithdopmean, null, "eviwithdopmean")


//print(final_col, "finalcolbeforedopdoh")
// add dop and doh to withcel main collection
var adddopdoh = function(image) {
  var adddopdohbands = eviwithdopmean.select("DOP_mean", "DOH_mean");
  return image.addBands(adddopdohbands);
};

var final_col = eviwithdopmaxmin.map(adddopdoh);
//print(final_col, "finalcolafterdopdoh")
var final_col = final_col.map(addDate)

// Calculate DAP
var calculateDAP = function(image) {
  var DAP= image.expression(
    'DayOfYear - DOP_mean', {
      'DayOfYear': image.select("doy"),
      'DOP_mean': image.select("DOP_mean"),
      
    }).rename('DAP');
  return image.addBands(DAP)
};
var final_col = final_col.map(calculateDAP);
//Map.addLayer(final_col, null, "final_col")

//Modeled LUEMax

// subtract Dop from DOY
var addDAP = function(image) {

  var DAP = ee.Image().expression(
    "((i.doy - i.DOP_mean) < 0.0) ? 0.0"+
    ":i.doy - i.DOP_mean", { i: image}
  ).rename('DAP_1')
  
  return image.addBands(DAP)  
 }

var withDAPfromDOP = final_col.map(addDAP)
print(withDAPfromDOP, "withDAPfromDOP")
//Map.addLayer( withDAPfromDOP)


// subtract Dop from DOY
var addDAP2 = function(image) {

  var DAP = ee.Image().expression(
    "((i.DOH_mean - i.doy) < 0.0) ? 0.0"+
    ":i.doy - i.DOP_mean", { i: image}
  ).rename('DAP_2')
  
  return image.addBands(DAP)  
 }
var withDAPfromDOP = withDAPfromDOP.map(addDAP2)
 
// subtract Dop from DOY
var addDAP3 = function(image) {

  var DAP3 = ee.Image().expression(
    "((i.DAP_2) < 0.0) ? 0.0"+
    ":i.DAP_2", { i: image}
  ).rename('DAP_3')
  
  return image.addBands(DAP3)  
 }


var withDAPfromDOP = withDAPfromDOP.map(addDAP3)
print("withDAPfromDOP", withDAPfromDOP)
//print(withDAPfromDOP, "withDAPfromDOP")
// Map.addLayer( withDAPfromDOP)


// calculate the GDD
// GDD
var dataset = ee.ImageCollection('OREGONSTATE/PRISM/AN81d')
                  .filter(ee.Filter.date('2020-01-01', '2020-12-31'));

// Set Tmax greater than 30 as 30
var temp30 = function(image) {

  var tmax_30 = ee.Image().expression(
    "((i.tmax) >= 30.0) ? 30.0"+
    ":i.tmax", { i: image}
  ).rename('tmax_30')
  
  return image.addBands(tmax_30)  
 }


var tmax_30_collection = dataset.map(temp30);
// print(tmax_30_collection, "tmax_30_collection");
// Map.addLayer( tmax_30_collection);

// 
// Calculate t
var gddplustenfunc = function(image) {
  var tmax = image.select("tmax_30")
  var tmin = image.select("tmin")
  var GDDplusten = ((tmax.add(tmin)).divide(2)).rename('GDDplusten')
  return image.addBands(GDDplusten);
};

var gddplusten_collection = tmax_30_collection.map(gddplustenfunc);
// print(gddplusten_collection);


// subtract Dop from DOY
var addgdd = function(image) {

  var gdd = ee.Image().expression(
    "((i.GDDplusten) < 10.0) ? 0.0"+
    ":i.GDDplusten - 10", { i: image}
  ).rename('gdd')
  
  return image.addBands(gdd)  
 }

var gdd_collection = gddplusten_collection.map(addgdd);

// print(gdd_collection);

// The gdd is calculated at one day. To calculate the Cumulative GDD this is the right
//  time . because cumulative GDD needs to be calculated at one day interval 
// add dop to the gdd collection
var gdd_collection = gdd_collection.map(addDate)
var gdd_collection = gdd_collection.map(floatcollection)
var gdd_collection = gdd_collection.map(adddopdoh);
var gdd_collection = gdd_collection.map(calculateDAP);
var gdd_collection = gdd_collection.map(addDAP)
var gdd_collection = gdd_collection.map(addDAP2)
var gdd_collection = gdd_collection.map(addDAP3)


// eliminate the non growing season gdd values
// subtract Dop from DOY
var nonzerogdd = function(image) {
  var GDDnz = ee.Image().expression(
    "((i.DAP_3) == 0.0) ? 0.0"+
    ":i.gdd", { i: image}
  ).rename('GDDnz')
  return image.addBands(GDDnz)  
 }
 
var gdd_collection = gdd_collection.map(nonzerogdd)
// we have gdd 
// we need cumulative gdd
// calculate the sum of GGD values
// var gdd_collection = gdd_collection.select('GDDnz').sum().rename('summedGDD');

var time0 = gdd_collection.first().get('system:time_start');
// print("time0", time0)
// The first anomaly image in the list is just 0, with the time0 timestamp.
var first = ee.List([
  // Rename the first band 'EVI'.
  ee.Image(0).set('system:time_start', time0).select([0], ['GDDnz'])
]);

// print("first", first)
// Create an ImageCollection of cumulative anomaly images by iterating.
// Since the return type of iterate is unknown, it needs to be cast to a List.
// This is a function to pass to Iterate().
// As anomaly images are computed, add them to the list.
var accumulate = function(image, list) {
  // Get the latest cumulative anomaly image from the end of the list with
  // get(-1).  Since the type of the list argument to the function is unknown,
  // it needs to be cast to a List.  Since the return type of get() is unknown,
  // cast it to Image.
  var previous = ee.Image(ee.List(list).get(-1));
  // Add the current anomaly to make a new cumulative anomaly image.
  var added = image.add(previous)
    // Propagate metadata to the new image.
    .set('system:time_start', image.get('system:time_start'));
  // Return the list with the cumulative anomaly inserted.
  return ee.List(list).add(added);
};

var cumulative = ee.ImageCollection(ee.List(gdd_collection.iterate(accumulate, first)));
// print(cumulative, "cumulative")
// Filter out the first image
var filtered = cumulative.filter(ee.Filter.inList('system:index',['0']).not());
var filtered_gdd= gdd_collection.select('GDDnz');
// print(filtered);
// Map.addLayer(filtered_gdd, null, "filtered_gdd");



// Filter out the first image
var filtered = cumulative.filter(ee.Filter.inList('system:index',['0']).not());
var filtered_gdd= filtered.select('GDDnz');
// print("filtered", filtered);
// Map.addLayer(filtered_gdd, null, "filtered_gdd");
// Map.addLayer(filtered_gdd, null, "filtered_gdd");
//  The second filtered gdd has the cumulative GDD

// daily gdd to 8 day gdd
var eightday = filtered_gdd.filterDate('2020-01-01', "2020-12-31")
// print("eightday", eightday)

var startDate = ee.Date('2020-01-01')
var endDate = ee.Date('2020-12-31')
var dayOffsets = ee.List.sequence(
  0, 
  endDate.difference(startDate, 'days').subtract(1),
  8 // Single day every week
)

var weeklyMeansgdd = ee.ImageCollection.fromImages(
  dayOffsets.map(function(dayOffset) {
    var start = startDate.advance(dayOffset, 'day')
    var end = start.advance(8, 'day')
    return eightday
      .filterDate(start, end)
      .mean()
      .set('system:time_start', start.millis());
  })  
);
print('weeklyMeansgdd', weeklyMeansgdd)
// Map.addLayer(weeklyMeansgdd, null, "weeklyMeansgdd")

// Combine GDD and EVI
var mod1 = withDAPfromDOP
var mod2 = weeklyMeansgdd

var filter = ee.Filter.equals({
  leftField: 'system:time_start',
  rightField: 'system:time_start'
});

// Create the join.
var simpleJoin = ee.Join.inner();

// Inner join
var innerJoin = ee.ImageCollection(simpleJoin.apply(mod1, mod2, filter))

var withDAPfromDOPwithGDD = innerJoin.map(function(feature) {
  return ee.Image.cat(feature.get('primary'), feature.get('secondary'));
})

// print('withDAPfromDOPwithGDD', withDAPfromDOPwithGDD)
// Map.addLayer(withDAPfromDOPwithGDD, null, "withDAPfromDOPwithGDD")

// remove the post harvest gdd or convert them to 0
var zeroharvestGDD = function(image) {

  var GDDnzh = ee.Image().expression(
    "((i.DAP_3) == 0.0) ? 0"+
    ":i.GDDnz", { i: image}
  ).rename('GDDnzh')
  
  return image.addBands(GDDnzh)  
 }
var withDAPfromDOPwithGDD = withDAPfromDOPwithGDD.map(zeroharvestGDD)

print("withDAPfromDOPwithGDD", withDAPfromDOPwithGDD);



// Finding the growing season maximum LSWI
var growingseasonLSWI = function(image) {

  var LSWI_INP_GS = ee.Image().expression(
    "((i.DAP_3) == 0.0) ? 0"+ // remove the post harvest gdd or convert them to 0
    ":i.LSWI_INP", { i: image}
  ).rename('LSWI_INP_GS')
  
  return image.addBands(LSWI_INP_GS)  
 }
var withDAPfromDOPwithGDD = withDAPfromDOPwithGDD.map(growingseasonLSWI)

print("withDAPfromDOPwithGDDwithgrowingseasonLSWI", withDAPfromDOPwithGDD);




// all parameter functions
// Calculate FAPAR SG
var allparamfunction = function(image) {
  var dswr = image.select("DSWR")
  var dswr_cal = dswr.multiply(0.9)//DSWR calibrated based on the site DSWR dataset on 8 day average
  var par = dswr_cal.multiply(2.02).multiply(0.0864).rename('par') //par conversion from DSWR following Zhang et al., 2017 method
  var tmax = image.select("Tmax")
  var tmaxcel = tmax.subtract(273.15).rename('tmaxcel');//Max Temperature conversion to deg celsius
  var temp = image.select("Temp")
  var tempcel = temp.subtract(273.15).rename('tempcel')//Mean Temperature conversion to deg celsius
  var tmean = tmaxcel.add(tempcel).divide(2). rename("tmean") //Mean Temperature calculated from the average of tmax and tmean
  var evi = image.select("EVI_SG"); //EVI for FAPAR calculation on next line
  var fapar = (evi.subtract(0.1)).multiply(1.25).rename('FAPAR')//FAPAR for VPM and PVPM
  var vpmLUEmax = (ee.Image.constant(0.6)).rename('vpmLUEmax'); //LUE for VPM
  return image.addBands(par).addBands(tmaxcel).addBands(tempcel).addBands(tmean).addBands(fapar).addBands(vpmLUEmax);
};

var allparamsimgcoll = withDAPfromDOPwithGDD.map(allparamfunction)
print("allparamsimgcoll", allparamsimgcoll);


// get the zeroes out
function fix_mask(image) {
  var mask = image.neq(0);
  return image.updateMask(mask);
}

var allparamsimgcoll_growing_season = allparamsimgcoll.map(fix_mask);
print("allparamsimgcoll_growing_season", allparamsimgcoll_growing_season);

var timeSeries = allparamsimgcoll_growing_season.map(function (image) {
  var date = image.date().format('yyyy-MM-dd');
  var values = image
    .clip(Way3)
    .reduceRegion({
      reducer: ee.Reducer.mean(),
      geometry: Way3,
      scale: 30
    });

  return ee.Feature(null, {
    date: date,
    EVI_SG: values.get('EVI_SG'),
    FAPAR: values.get('FAPAR'),
    LSWI_INP: values.get('LSWI_INP')
  });
});

// Create and print the chart.
print(ui.Chart.feature.byFeature(timeSeries, 'date', ['EVI_SG', 'FAPAR', 'LSWI_INP']));

// Export the time-series as a csv.
Export.table.toDrive({
  collection: timeSeries,
  description: 'Way3_2020',
  selectors: ['date', 'EVI_SG', 'FAPAR', 'LSWI_INP'],
  fileFormat: 'CSV'
});



// Plotting site calibrated Topt and LUEmax for the PVPM model



// Plottings to check
// Display a time-series chart
var chart = ui.Chart.image.series({
  imageCollection: MODISimageswithbands.select('EVI'),
  region: Way3,
  reducer: ee.Reducer.mean(),
  scale: 500
}).setOptions({
      title: 'EVI Raw data',
      interpolateNulls: false,
      vAxis: {title: 'EVI', viewWindow: {min: -1, max: 1}},
      hAxis: {title: '', format: 'YYYY-MM'},
      lineWidth: 1,
      pointSize: 4,
      series: {
        0: {color: '#238b45'},
      },
    })
print(chart);

// Display a time-series chart
var chart = ui.Chart.image.series({
  imageCollection: mod_ndvi_interp.select('MOD_NDVI_INTER'),
  region: Way3,
  reducer: ee.Reducer.mean(),
  scale: 500
}).setOptions({
      title: 'EVI Interpolated for a rice field',
      interpolateNulls: false,
      vAxis: {title: 'EVI Interpolated', viewWindow: {min: -1, max: 1}},
      hAxis: {title: '', format: 'YYYY-MM'},
      lineWidth: 1,
      pointSize: 4,
      series: {
        0: {color: '#238b45'},
      },
    })
print(chart);

// Display a time-series chart
var chart = ui.Chart.image.series({
  imageCollection: syn_series_sg.select('MOD_NDVI_SG'),
  region: Way3,
  reducer: ee.Reducer.mean(),
  scale: 500
}).setOptions({
      title: 'EVI Savitzkly Golay for a rice field',
      interpolateNulls: false,
      vAxis: {title: 'EVI Savitzkly Golay', viewWindow: {min: -1, max: 1}},
      hAxis: {title: '', format: 'YYYY-MM'},
      lineWidth: 1,
      pointSize: 4,
      series: {
        0: {color: '#238b45'},
      },
    })
print(chart);

// Display a time-series chart
var chart = ui.Chart.image.series({
  imageCollection: final_col.select('EVI_SG'),
  region: Morris,
  reducer: ee.Reducer.mean(),
  scale: 500
}).setOptions({
      title: 'Final Col EVI SG to Check with Final Collection Morris',
      interpolateNulls: false,
      vAxis: {title: 'EVI Savitzkly Golay', viewWindow: {min: -1, max: 1}},
      hAxis: {title: '', format: 'YYYY-MM'},
      lineWidth: 1,
      pointSize: 4,
      series: {
        0: {color: '#238b45'},
      },
    })
print(chart);

// LSWI gapfill//
var mod_evi = MODISimageswithbands.select(['LSWI'])
var interl_m = require('users/Yang_Chen/GF-SG:Interpolation_v1');
var frame  = 8*7; 
var nodata = -9999; 
var mod_ndvi_interp = interl_m.linearInterp(mod_evi, frame, nodata);
print(mod_ndvi_interp, "mod_ndvi_interp")
// print("mod_ndvi_interp", mod_ndvi_interp)
var mod_ndvi_interp0 = mod_ndvi_interp.select(['MOD_NDVI_INTER']);
// print("mod_ndvi_interp0", mod_ndvi_interp0)
print(mod_ndvi_interp0, "mod_ndvi_interp0_LSWI")

// Display a time-series chart
var chart = ui.Chart.image.series({
  imageCollection: mod_ndvi_interp0.select('MOD_NDVI_INTER'),
  region: Way3,
  reducer: ee.Reducer.mean(),
  scale: 500
}).setOptions({
      title: 'Raw data',
      interpolateNulls: false,
      vAxis: {title: 'LSWI gapfilled', viewWindow: {min: -1, max: 1}},
      hAxis: {title: '', format: 'YYYY-MM'},
      lineWidth: 1,
      pointSize: 4,
      series: {
        0: {color: '#238b45'},
      },
    })
print(chart);

var gddnzcumchart = ui.Chart.image.series({
  imageCollection: withDAPfromDOPwithGDD.select('GDDnzh'),
  region: Way3,
  reducer: ee.Reducer.mean(),
  scale: 500
}).setOptions({
      title: 'GDD cumulative',
      interpolateNulls: false,
      vAxis: {title: 'GDD cumulative'},
      hAxis: {title: '', format: 'YYYY-MM'},
      lineWidth: 1,
      pointSize: 4,
      series: {
        0: {color: '#238b45'},
      },
    })
print(gddnzcumchart);


// checking image collection after temperature data joining,  
var LSWIgrowing = ui.Chart.image.series({
  imageCollection: withDAPfromDOPwithGDD.select('LSWI_INP_GS'),
  region: Way3,
  reducer: ee.Reducer.mean(),
  scale: 500
}).setOptions({
      title: 'LSWI growing season',
      interpolateNulls: false,
      vAxis: {title: 'LSWI from the growing season'},
      hAxis: {title: '', format: 'YYYY-MM'},
      lineWidth: 1,
      pointSize: 4,
      series: {
        0: {color: '#238b45'},
      },
    })
print(LSWIgrowing);

// Imports:
var RiceArkansas2020 = ee.FeatureCollection("users/riasadmahbub/RiceArkansas2020"),
    Baker20 = /* color: #d63000 */ee.Geometry.Point([-91.7461338646564, 34.5838519487694]),
    Baker30 = /* color: #98ff00 */ee.Geometry.Point([-91.7457309964605, 34.5798268558294]),
    Baker40 = /* color: #0b4a8b */ee.Geometry.Point([-91.7413897921733, 34.5833351349677]),
    Baker50 = /* color: #ffc82d */ee.Geometry.Point([-91.7415124345687, 34.5800866172044]),
    BottofRes = /* color: #00ffff */ee.Geometry.Point([-91.7408865406103, 34.5867739135253]),
    BransfordEast = /* color: #bf04c2 */ee.Geometry.Point([-91.7136487875642, 34.5745969950903]),
    CarrNorth = /* color: #ff0000 */ee.Geometry.Point([-91.7135062390426, 34.5936479486112]),
    CarrSouth = /* color: #00ff00 */ee.Geometry.Point([-91.7142243528067, 34.5905757609019]),
    Flat = /* color: #0000ff */ee.Geometry.Point([-91.7081567976966, 34.5970313211982]),
    Frog40 = /* color: #999900 */ee.Geometry.Point([-91.7628871549134, 34.5898473722021]),
    Haley = /* color: #009999 */ee.Geometry.Point([-91.7054855864044, 34.5790559332357]),
    Judys = /* color: #ff00ff */ee.Geometry.Point([-91.7534407121582, 34.6251396169254]),
    Kelly1 = /* color: #ff9999 */ee.Geometry.Point([-91.7358805847778, 34.5801425985041]),
    Kelly10 = /* color: #99ff99 */ee.Geometry.Point([-91.7283989295389, 34.5741139021514]),
    Kelly11 = /* color: #9999ff */ee.Geometry.Point([-91.7251365154376, 34.5739072272054]),
    Kelly2 = /* color: #ffff99 */ee.Geometry.Point([-91.7366598930216, 34.5839946460025]),
    Kelly3 = /* color: #99ffff */ee.Geometry.Point([-91.7621828811702, 34.5946581659088]),
    Kelly4 = /* color: #ff99ff */ee.Geometry.Point([-91.7306795753099, 34.5794527478732]),
    Kelly6 = /* color: #d63000 */ee.Geometry.Point([-91.7409485282106, 34.5761107541391]),
    Kelly5 = /* color: #98ff00 */ee.Geometry.Point([-91.7269969640149, 34.5796306004904]),
    Kelly7 = /* color: #0b4a8b */ee.Geometry.Point([-91.7370784435666, 34.5744958258363]),
    Kelly8 = /* color: #ffc82d */ee.Geometry.Point([-91.7321440958848, 34.5743493449877]),
    Md40West = /* color: #00ffff */ee.Geometry.Point([-91.7626422745471, 34.5862059408305]),
    Mid40Hwy = /* color: #bf04c2 */ee.Geometry.Point([-91.7591777285449, 34.587527239902]),
    MidBransford = /* color: #ff0000 */ee.Geometry.Point([-91.7166403663427, 34.5740537896786]),
    Morris = /* color: #00ff00 */ee.Geometry.Point([-91.7131339503699, 34.5796865756611]),
    NewGround = /* color: #0000ff */ee.Geometry.Point([-91.7408061240978, 34.5911016809739]),
    Pops = /* color: #999900 */ee.Geometry.Point([-91.709634216807, 34.5914656356362]),
    SIShop = /* color: #009999 */ee.Geometry.Point([-91.7116655391847, 34.5865424819934]),
    The90 = /* color: #ff00ff */ee.Geometry.Point([-91.75854615797722, 34.59219742642233]),
    TopofRes = /* color: #ff9999 */ee.Geometry.Point([-91.7448956614916, 34.5879077618286]),
    Walls11 = /* color: #99ff99 */ee.Geometry.Point([-91.7631004243578, 34.5952230564873]),
    Walls3 = /* color: #9999ff */ee.Geometry.Point([-91.771296100729, 34.5906692897482]),
    Walls5 = /* color: #ffff99 */ee.Geometry.Point([-91.767146487485, 34.5901601818756]),
    Way1 = /* color: #99ffff */ee.Geometry.Point([-91.753456055996, 34.595186260389]),
    Way2 = /* color: #ff99ff */ee.Geometry.Point([-91.7518049230273, 34.5915087541966]),
    Way3 = /* color: #d63000 */ee.Geometry.Point([-91.7520204989952, 34.5871484192842]),
    Way4 = /* color: #98ff00 */ee.Geometry.Point([-91.7512638777895, 34.583608061697]),
    Way5 = /* color: #0b4a8b */ee.Geometry.Point([-91.750856194307, 34.5802569634905]),
    OF2 = /* color: #00ffff */ee.Geometry.Point([-90.0489, 35.7406]),
    OF3 = /* color: #bf04c2 */ee.Geometry.Point([-90.0444, 35.7372]),
    OF4 = /* color: #ff0000 */ee.Geometry.Point([-90.0381, 35.7344]),
    OF5 = /* color: #00ff00 */ee.Geometry.Point([-90.0406, 35.7297]),
    OF6 = /* color: #0000ff */ee.Geometry.Point([-90.0403, 35.7333]),
    USBDA = /* color: #999900 */ee.Geometry.Point([-90.0327, 35.8089]),
    USBDC = /* color: #009999 */ee.Geometry.Point([-90.0284, 35.8089]),
    OF1 = /* color: #d63000 */ee.Geometry.Point([-90.0492, 35.737067]);
