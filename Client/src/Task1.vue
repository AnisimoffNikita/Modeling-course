<template>
  <div id="task1">
    <b-container class="text-dark text-left my-3">
      <b-row>
        <b-col md="3">Пикара N</b-col>
        <b-col md="3">
          <b-form-input type="text" v-model="n"></b-form-input>
        </b-col>
      </b-row>
      <b-row class="my-3">
        <b-col md="3">Шаг</b-col>
        <b-col md="3">
          <b-form-input type="text" v-model="h"></b-form-input>
        </b-col>
      </b-row>
      <b-row class="my-3">
        <b-col md="3">Максимальный х</b-col>
        <b-col md="3">
          <b-form-input type="text" v-model="xs"></b-form-input>
        </b-col>
      </b-row>
      <b-row class="my-3">
        <b-col md="3">Точность</b-col>
        <b-col md="3">
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
      <b-row >
        <b-col>
          <b-pagination :total-rows="items.length" :per-page="perPage" v-model="currentPage" />
        </b-col>
      </b-row>
      <b-row  class="mb-3">
        <b-col>
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
        </b-col>
      </b-row>
    </b-container>
  </div>
</template>

<script>



export default {
  name: 'task1',
  data () {
    return {
      fields: [ 'х', "Эйлера (явный)", 'Пикара (3)', 'Пикара (5)', 'Пикара (n)', 'Рунге-Кутта (2)', 'Рунге-Кутта (4)'
              ],
      items: [],
      n: 5,
      h: 1e-1,
      xs: 2,
      loading: false,
      error: null,
      currentPage: 1,
      perPage: 100,
      acc: 4
    }
  },
  methods: {
    get:function(){
      console.log("fetching data")
    },
    fetchData: function () {
      this.error = null
      this.loading = true

      var s = `http://localhost:8081/task1/${this.n}/${this.h}/${this.xs}`
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
              console.log(this.fields);
              for (var i = 0; i < json.length; i++) {
                var obj = {};
                for (var j = 0; j < json[i].length; j++){
                  obj[this.fields[j]] = json[i][j];
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