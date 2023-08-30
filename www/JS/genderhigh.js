 // This isn't strictly necessary, but it's good JavaScript hygiene.
//(function() {

var binding = new Shiny.OutputBinding();

binding.find = function(scope) {
  // For the given scope, return the set of elements that belong to
  // this binding.
  return $(scope).find(".genderhigh");
};

binding.renderValue = function(el, data) {
  // This function will be called every time we receive new output
  // values for a line chart from Shiny. The "el" argument is the
  // div for this particular chart.
  
  var $el = $(el);
  
  //ajustando os dados aos parâmetros do gráfico (01-fev-2019)
  var teste = data;
  /*console.log(teste);*/
  /*var categories = teste[0];*/

 
 /* Object.keys(teste).forEach(function(key) {
  teste[key].unshift(key);
});
  */
 
   
 /*  console.log(teste);

 //para as categorias do eixo x
  var nome_x = teste['.id'];
  nome_x.shift();
*/
  //console.log(nome_x);
/*
 var empty = Array();
 for(i = 0, i = result.length)
 
  var x = [
            ['data1', 30, 200, 100, 400, 150, 250],
            ['data2', 130, 100, 140, 200, 150, 50]
        ];
  console.log(x);*/

 /*These lines are all chart setup.  Pick and choose which chart features you want to utilize. */
 Highcharts.chart(el, {

    chart: {
        type: 'bar',
        plotBackgroundColor: null
    },
    
    title: {
      text: null
    },

    xAxis: [{
        categories: teste[0],
        reversed: false,
        labels: {
            step: 1
        },
        accessibility: {
            description: 'Homens'
        }
    }, { // mirror axis on right side
        opposite: true,
        reversed: false,
        categories: teste[0],
        linkedTo: 0,
        labels: {
            step: 1
        },
        accessibility: {
            description: 'Mulheres'
        }
    }],

   yAxis: {
        title: {
            text: null
        },
        labels: {
            formatter: function () {
                return Math.abs(this.value);
            }
        },
        accessibility: {
            description: 'Número de pessoas'
        }
    },
        
       
     plotOptions: {
        series: {
            stacking: 'normal'
        },
         bar: {
              
              dataLabels: {
                        enabled: false
                    }
              }
    },
   
     tooltip: {
        formatter: function () {
            return '<b>' + this.series.name + ', com idade entre ' + this.point.category + '</b><br/>' +
                'Total: ' + Highcharts.numberFormat(Math.abs(this.point.y), 1) ;
        }
    },
    
    series: teste[1]

});


}

// Tell Shiny about our new output binding
Shiny.outputBindings.register(binding, "genderchart");
//}) //endfunction

