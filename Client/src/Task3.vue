<template>
  <div id="task1">
    <b-container class="text-dark text-left my-3">
      <b-row>
        <b-col md="2">θ</b-col>
        <b-col md="2">
          <b-form-input type="text" v-model="params.theta"></b-form-input>
        </b-col>
        <b-col md="2">λ0</b-col>
        <b-col md="2">
          <b-form-input type="text" v-model="params.lambda0"></b-form-input>
        </b-col>
        <b-col md="2">r</b-col>
        <b-col md="2">
          <b-form-input type="text" v-model="params.r"></b-form-input>
        </b-col>
      </b-row>

      <b-row>
        <b-col md="2">F0</b-col>
        <b-col md="2">
          <b-form-input type="text" v-model="params.f0"></b-form-input>
        </b-col>
        <b-col md="2">α0</b-col>
        <b-col md="2">
          <b-form-input type="text" v-model="params.alpha0"></b-form-input>
        </b-col>
        <b-col md="2">αN</b-col>
        <b-col md="2">
          <b-form-input type="text" v-model="params.alphaN"></b-form-input>
        </b-col>
      </b-row>

      <b-row>
        <b-col md="2">Toc</b-col>
        <b-col md="2">
          <b-form-input type="text" v-model="params.toc"></b-form-input>
        </b-col>
        <b-col md="2">power</b-col>
        <b-col md="2">
          <b-form-input  type="text" v-model="params.power"></b-form-input>
        </b-col>
      </b-row>


      <b-row class="my-3">
        <b-col md="2">l</b-col>
        <b-col md="2">
          <b-form-input type="text" v-model="params.l"></b-form-input>
        </b-col>
        <b-col md="2">h</b-col>
        <b-col md="2">
          <b-form-input type="text" v-model="params.h"></b-form-input>
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
                <line-plot :chart-data="T" ></line-plot>
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
      fields: [ 'x', 'T'],
      items: [],
      params : {
        theta : "293",
        lambda0 : "0.1",
        r : "0.5",
        f0 : "100",
        l : "10",
        alpha0 : "0.01",
        alphaN : "0.005",
        toc : "293",
        h : "0.001",
        power : "2"
      },
      
      perPage: 500,
      currentPage:1,
      acc: 6,
      loading: false,
      T: {
        labels:  null,
        datasets: [
          {
            label: 'T(x)',
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

      var s = `http://localhost:8081/task3`

      var postData = `theta=${this.params.theta}&`+
            `lambda0=${this.params.lambda0}&` +
            `r=${this.params.r}&`+
            `f0=${this.params.f0}&`+
            `l=${this.params.l}&`+
            `alpha0=${this.params.alpha0}&`+
            `alphaN=${this.params.alphaN}&`+
            `toc=${this.params.toc}&`+
            `h=${this.params.h}&`+
            `power=${this.params.power}`

      console.log(s, postData)

      fetch(s, {
              method: "POST",
              body: postData,
              headers: {  
                "Content-type": "application/x-www-form-urlencoded; charset=UTF-8"  
              },  
            })
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

              console.log(json)

              this.T = {
                labels:  json[0],
                datasets: [
                  {
                    label: 'T(x)',
                    backgroundColor: '#f87979',
                    borderColor: '#f87979',
                    data: json[1],
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