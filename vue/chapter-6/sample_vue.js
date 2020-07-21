import Vue from 'vue'
import Vuex from 'vuex'

Vue.use(Vuex)

function getCountNum(type) {
  return new Promise(resolve => {
    setTimeout(() => {
      let amount
      switch (type) {
        case 'one':
          amount = 1
          break
        case 'two':
          amount = 2
          break
        case 'ten':
          amount = 10
          break
        default:
          amount = 0
      }
      resolve({ amount })
    },1000)
  })
}

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
  },
  actions: {
    incrementAsync({ commit }, payload) {
      return getCountNum(payload.type)
        .then(data => {
          console.log(data)
          commit('increment', {
            amount: data.amount
          })
        })
    }
  }
})

console.log(store.state.count) // 0
store.commit('increment', 1)
console.log(store.state.count)

console.log(store.getters.cubed)

store.dispatch('incrementAsync', { type: 'one' })
