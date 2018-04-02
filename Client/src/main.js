import Vue from 'vue'
import VueRouter from 'vue-router' 
import BootstrapVue from 'bootstrap-vue'


import App from './App.vue'
import HeaderBar from './HeaderBar.vue'
import About from './About.vue'
import Task1 from './Task1.vue'
import Task2 from './Task2.vue'
import Task3 from './Task3.vue'
import NotFound from './NotFound.vue'


Vue.use(VueRouter);
Vue.use(BootstrapVue);

const routes = [
  { path: '/', component: About },
  { path: '/task1', component: Task1 },
  { path: '/task2', component: Task2 },
  { path: '/task3', component: Task3 },
  { path: '*', component: NotFound }
];

const router = new VueRouter({
    mode: 'history',
    routes: routes
});

new Vue({
  el: '#app',
  render: h => h(App),
  router: router
})
