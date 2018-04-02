<template>

    <b-navbar toggleable="md" type="dark" variant="dark">

      <b-navbar-toggle target="nav_collapse"></b-navbar-toggle>

      <b-navbar-brand href="#">Моделирование</b-navbar-brand>

      <b-collapse is-nav id="nav_collapse">

        <b-navbar-nav>

          <b-nav-item to="/" exact>Информация</b-nav-item>
          <b-nav-item to="/task1">ЛР 1</b-nav-item>
          <b-nav-item to="/task2">ЛР 2</b-nav-item>
          <b-nav-item to="/task3">ЛР 3</b-nav-item>

          <b-nav-item v-for="i in [4]" :key="i" href="#" disabled>ЛР {{i}}</b-nav-item>

        </b-navbar-nav>

      </b-collapse>
    </b-navbar>
</template>

<script>
export default {
  name: 'header-bar'
}
</script>

<style>
</style>