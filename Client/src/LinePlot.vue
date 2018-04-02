<template>
  <div>
  <canvas ref="canvas" ></canvas>
  </div>
</template>

<script>

import {Line, mixins} from 'vue-chartjs'

export default {
  name: 'line-plot',
  mixins: [mixins.reactiveProp],
  extends: Line,
  props: ['chartData', 'options'],
  data () {
    return {
      dataopts: {
          tooltips: {enabled: false},
          hover: {mode: null},
          responsive: true,
          maintainAspectRatio: true,
          steppedLine: true,
          scales: {
          xAxes: [{
            ticks: {
              callback: function(dataLabel, index) {
                return Number(dataLabel).toExponential(5);
              }
            },
            scaleLabel: {
              display: true,
              labelString: 't'
            }
          }],
          yAxes: [{
            ticks: {
              callback: function(dataLabel, index) {
                return Number(dataLabel).toExponential(5);
                
              }
            }
          }]
        }
      }
    }

  },
  mounted () {
    this.renderChart(this.chartData, this.dataopts);
  }
}
</script>

<style>
</style>
