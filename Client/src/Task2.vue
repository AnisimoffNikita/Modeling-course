<template>
  <div id="task1">
    <b-container class="text-dark text-left my-3">
      <b-row>
        <b-col md="2">Сk</b-col>
        <b-col md="2">
          <b-form-input type="text" v-model="params.ck"></b-form-input>
        </b-col>
        <b-col md="2">Lk</b-col>
        <b-col md="2">
          <b-form-input type="text" v-model="params.lk"></b-form-input>
        </b-col>
        <b-col md="2">rk</b-col>
        <b-col md="2">
          <b-form-input type="text" v-model="params.rk"></b-form-input>
        </b-col>
      </b-row>

      <b-row>
        <b-col md="2">U0</b-col>
        <b-col md="2">
          <b-form-input type="text" v-model="params.u0"></b-form-input>
        </b-col>
        <b-col md="2">I0</b-col>
        <b-col md="2">
          <b-form-input type="text" v-model="params.i0"></b-form-input>
        </b-col>
        <b-col md="2">Radius</b-col>
        <b-col md="2">
          <b-form-input type="text" v-model="params.radius"></b-form-input>
        </b-col>
      </b-row>

      <b-row>
        <b-col md="2">p0</b-col>
        <b-col md="2">
          <b-form-input type="text" v-model="params.p0"></b-form-input>
        </b-col>
        <b-col md="2">Tнач</b-col>
        <b-col md="2">
          <b-form-input type="text" v-model="params.tn"></b-form-input>
        </b-col>
        <b-col md="2">L</b-col>
        <b-col md="2">
          <b-form-input type="text" v-model="params.l"></b-form-input>
        </b-col>
      </b-row>

      <b-row class="my-3">
        <b-col md="2">dt</b-col>
        <b-col md="2">
          <b-form-input  type="text" v-model="dt"></b-form-input>
        </b-col>
        <b-col md="2">tmax</b-col>
        <b-col md="2">
          <b-form-input  type="text" v-model="tmax"></b-form-input>
        </b-col>
      </b-row>


      <b-row class="my-3">
        <b-col md="2">Точность</b-col>
        <b-col md="2">
          <b-form-input type="number" v-model="acc"></b-form-input>
        </b-col>
      </b-row>

      <b-row class="my-3">
        <b-col v-if="!loading" md="3">
          <b-button variant="primary" v-on:click="fetchData">
            Выполнить
          </b-button>
        </b-col>
        <b-col v-if="loading" md="3">
          <b-button disabled variant="primary" v-on:click="fetchData">
            Выполнение
          </b-button>
        </b-col>
      </b-row>
      <b-row  class="mb-3">
        <b-col md="12">
          <b-card no-body>
            <b-tabs pills card>
              <b-tab title="Графики" active>
                <b-col md="12">
                <line-plot :chart-data="I" :options="dataopts"></line-plot>
                </b-col>
                <b-col md="12">
                <line-plot :chart-data="U" :options="dataopts"></line-plot>
                </b-col>
                <b-col md="12">
                <line-plot :chart-data="R" :options="dataopts"></line-plot>
                </b-col>
              </b-tab>
              <b-tab title="Значения">
                <b-pagination :total-rows="items.length" :per-page="perPage" v-model="currentPage" />
                <b-table bordered 
                        small fixed hover 
                        :current-page="currentPage"
                        :per-page="perPage" 
                        :items="items" 
                        :fields="fields"
                        class="text-right">
                  <template v-for="f in fields" :slot="f" slot-scope="data">
                    {{data.value | round(acc)}}
                  </template>
                </b-table>
              </b-tab>
            </b-tabs>
          </b-card>
        </b-col>
      </b-row>
    </b-container>
  </div>
</template>

<script>

import LinePlot from './LinePlot.vue'

export default {
  components: {
    LinePlot
  },
  name: 'task2',
  data () {
    return {
      fields: [ 't', 'I', 'U', 'R'],
      items: [],
      params : {
        rk: "0.2",
        lk:"60e-6",
        ck: "150e-6",
        u0: "3000",
        i0: "0",
        radius: "0.35",
        p0:"0.5",
        tn:"300",
        l:"12"
      },
      
      dt: "1e-6",
      tmax:"100e-6",
      perPage: 500,
      currentPage:1,
      acc: 6,
      loading: false,
      I: {
        labels:  null,
        datasets: [
          {
            label: 'I(t)',
            backgroundColor: '#f87979',
            borderColor: '#f87979',
            data: null,
            pointRadius: 0,
            pointHoverRadius: 0
          }
        ]
      },

      U: {
        labels:  null,
        datasets: [
          {
            label: 'U(t)',
            backgroundColor: '#f87979',
            borderColor: '#f87979',
            data: null,
            pointRadius: 0,
            pointHoverRadius: 0
          }
        ]
      },

      R: {
        labels:  null,
        datasets: [
          {
            label: 'R(t)',
            backgroundColor: '#f87979',
            borderColor: '#f87979',
            data: null,
            pointRadius: 0,
            pointHoverRadius: 0
          }
        ]
      }
    }
  },
  methods: {
    fetchData: function () {
      this.error = null
      this.loading = true

      var s = `http://localhost:8081/task2` + 
              `/${this.params.rk}` +
              `/${this.params.lk}` +
              `/${this.params.ck}` +
              `/${this.params.u0}` +
              `/${this.params.i0}` +
              `/${this.params.radius}` +
              `/${this.params.p0}` +
              `/${this.params.tn}` +
              `/${this.params.l}` +
              `/${this.dt}` +
              `/${this.tmax}`

      console.log(s)

      fetch(s)
            .then((response) => {
              if(response.ok) {
                return response.json();
              }
              throw new Error('Network response was not ok');
            })
            .then((json) => {
              var result = []
              var n = json.length;
              var m = json[0].length;

              this.I = {
                labels:  json[0],
                datasets: [
                  {
                    label: 'I(t)',
                    backgroundColor: '#f87979',
                    borderColor: '#f87979',
                    data: json[1],
                    fill: false,
                    pointRadius: 0,
                    pointHoverRadius: 0

                  }
                ]
              };
              this.U = {
                labels:  json[0],
                datasets: [
                  {
                    label: 'U(t)',
                    backgroundColor: '#f87979',
                    borderColor: '#f87979',
                    data: json[2],
                    fill: false,
                    pointRadius: 0,
                    pointHoverRadius: 0

                  }
                ]
              };
              this.R = {
                labels:  json[0],
                datasets: [
                  {
                    label: 'R(t)',
                    backgroundColor: '#f87979',
                    borderColor: '#f87979',
                    data: json[3],
                    fill: false,
                    pointRadius: 0,
                    pointHoverRadius: 0

                  }
                ]
              };

              for (var i = 0; i < m; i++) {
                var obj = {};
                for (var j = 0; j < n; j++){
                  obj[this.fields[j]] = json[j][i];
                }
                result.push(obj)
              }
              this.loading = false
              this.items = result;
            })
            .catch((error) => {
              this.error = error.toString()
              console.log(error);
            });
    }

  },
  filters: {
    round(value, decimals) {
      if(!value) {
        value = 0;
      }

      if(!decimals) {
        decimals = 0;
      }

      value = Math.round(value * Math.pow(10, decimals)) / Math.pow(10, decimals);
      return value;
    }
  }
}
</script>

<style>
</style>