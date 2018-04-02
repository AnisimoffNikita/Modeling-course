<template>
  <div id="app">
    <header-bar></header-bar>
    <router-view></router-view>
    <sticky-footer></sticky-footer>
  </div>
</template>

<script>
import HeaderBar from './HeaderBar.vue'
import StickyFooter from './StickyFooter.vue'

export default {
  name: 'app',
  data () {
    return {
      msg: 'Welcome to Your Vue.js App'
    }
  },
  components: {
    HeaderBar,
    StickyFooter
  }
}
</script>

<style>
@media (min-width: 1800px) {
    .container{
        max-width: 1600px;
    }
}
</style>
