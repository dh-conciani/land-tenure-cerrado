// get land cover and land use (mapbiomas collection 7) per land tenure and state
// for the cerrado biome 
// dhemerson.costa@ipam.org.br

// read land-tenure
var tenure = ee.Image('users/mapbiomascerrado1/fundiario_ipam/fundiario');

// get no information
var no_info = tenure.updateMask(tenure.eq(0)).aside(Map.addLayer);

// read mapbiomas collection 7
var mapbiomas = ee.Image('projects/mapbiomas-workspace/public/collection7/mapbiomas_collection70_integration_v2')
      // clip by reference
      .updateMask(tenure);

// read states
var states = ee.Image('projects/mapbiomas-workspace/AUXILIAR/estados-2016-raster')
      // clip by reference
      .updateMask(tenure);
      
Map.addLayer(tenure.randomVisualizer(), {}, 'tenure');
Map.addLayer(mapbiomas.select(['classification_2021']), mapb_pal, 'mapbiomas');
Map.addLayer(states.randomVisualizer(), {}, 'states');






















// import mapbiomas palette
var mapb_pal = {'min': 0,
                'max': 49,
                'palette': require('users/mapbiomas/modules:Palettes.js')
                .get('classification6')
              };
