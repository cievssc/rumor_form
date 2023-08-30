 // This isn't strictly necessary, but it's good JavaScript hygiene.
//(function() {

var binding = new Shiny.OutputBinding();

binding.find = function(scope) {
  // For the given scope, return the set of elements that belong to
  // this binding.
  return $(scope).find(".bubblehigh");
};

binding.renderValue = function(el, data) {
  // This function will be called every time we receive new output
  // values for a line chart from Shiny. The "el" argument is the
  // div for this particular chart.
  
  var $el = $(el);
  
  //ajustando os dados aos parâmetros do gráfico (01-fev-2019)
  var teste = data;
 //console.log(teste);
 

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
        type: 'bubble',
        plotBorderWidth: 1,
        zoomType: 'xy'
    },

    legend: {
        enabled: false
    },

    title: {
        text: null
    },

    subtitle: {
        text: null
    },

    /*accessibility: {
        point: {
            valueDescriptionFormat: '{index}. {point.name}, fat: {point.x}g, sugar: {point.y}g, obesity: {point.z}%.'
        }
    },*/

    xAxis: {
        gridLineWidth: 1,
        title: {
            text: 'Letalidade'
        },
        labels: {
            format: '{value} %'
        }/*,
         plotLines: [{
            color: 'black',
            dashStyle: 'dot',
            width: 2,
            value: 65,
            label: {
                rotation: 0,
                y: 15,
                style: {
                    fontStyle: 'italic'
                },
                text: 'Safe fat intake 65g/day'
            },
            zIndex: 3
        }],
        accessibility: {
            rangeDescription: 'Range: 60 to 100 grams.'
        }*/
    },

    yAxis: {
        startOnTick: false,
        endOnTick: false,
        title: {
            text: 'Incidência de casos (100.000 hab.)'
        }/*,
        labels: {
            format: '{value} gr'
        },
        maxPadding: 0.2,
       /* plotLines: [{
            color: 'black',
            dashStyle: 'dot',
            width: 2,
            value: 50,
            label: {
                align: 'right',
                style: {
                    fontStyle: 'italic'
                },
                text: 'Safe sugar intake 50g/day',
                x: -10
            },
            zIndex: 3
        }],
        accessibility: {
            rangeDescription: 'Range: 0 to 160 grams.'
        }*/
    },

    tooltip: {
        useHTML: true,
        headerFormat: '<table>',
        pointFormat: '<tr><th colspan="2"><h4>{point.country}</h4></th></tr>' +
            '<tr><th>Letalidade:</th><td>{point.x}%</td></tr>' +
            '<tr><th>Incidência:</th><td>{point.y} p/ 100.000 hab.</td></tr>' +
            '<tr><th>Óbitos:</th><td>{point.z}</td></tr>',
        footerFormat: '</table>',
        followPointer: true
    },

    plotOptions: {
        series: {
            dataLabels: {
                enabled: true,
                format: '{point.name}'
            }
        }
    },

    series: [
        { data: teste}
    ]

});

}

// Tell Shiny about our new output binding
Shiny.outputBindings.register(binding, "bubblechart");
//}) //endfunction

