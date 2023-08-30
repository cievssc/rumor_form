 // This isn't strictly necessary, but it's good JavaScript hygiene.
//(function() {

var binding = new Shiny.OutputBinding();

binding.find = function(scope) {
  // For the given scope, return the set of elements that belong to
  // this binding.
  return $(scope).find(".testeshiny");
};

binding.renderValue = function(el, data) {
  // This function will be called every time we receive new output
  // values for a line chart from Shiny. The "el" argument is the
  // div for this particular chart.
  
  var $el = $(el);

  var teste = data;
  Object.keys(teste).forEach(function(key) {
  teste[key].unshift(key);
});
  
  var result = Object.keys(teste).map(function(key) {
  return teste[key];
});

 // console.log(result);
/*
 var empty = Array();
 for(i = 0, i = result.length)
 
  var x = [
            ['data1', 30, 200, 100, 400, 150, 250],
            ['data2', 130, 100, 140, 200, 150, 50]
        ];
  console.log(x);*/

 /*These lines are all chart setup.  Pick and choose which chart features you want to utilize. */
  var chart = c3.generate({
    bindto: el,
    data: {
        x: '.id',
        columns: result,
        type: 'area-spline',
        axes: {
          valor_medio: 'y2'
        },
        transition: {
        duration: 1000
    }
    },
    point: {
        show: false
    	},
    axis: {
        x: {
            type: 'timeseries',
            tick: {
                format: '%m-%Y'
            }
         },
      y2: {
        show: true,
        min: '18000'
        },
      y: {
      	min: '35'
      }
      },
    zoom: {
        enabled: true,
    }
     
});

return chart

}; //render.binding
 /*console.log(binding);*/


// Tell Shiny about our new output binding
Shiny.outputBindings.register(binding, "shinyjsexamples.testeshiny");
//}) //endfunction

