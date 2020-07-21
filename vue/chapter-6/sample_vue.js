import Vue from 'vue'
import Vuex from 'vuex'

Vue.use(Vuex)

const store = new Vuex.Store({
  state: {
    count: 0
  },
  getters: {
    squared: (state) => state.count * state.count,
    cubed: (state, getters) => state.count * getters.squared
  },
  mutations: {
    increment(state, amount) {
      state.count += amount
    }
  }
})

console.log(store.state.count) // 0
store.commit('increment', 1)
console.log(store.state.count)
