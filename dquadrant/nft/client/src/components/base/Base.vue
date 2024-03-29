<template>
  <div>
    <header>
      <b-progress :max="100" height="0.1rem" style="background: transparent" animated>
        <b-progress-bar ref="progress" :value="this.loadingProgress" :variant="variant"></b-progress-bar>
      </b-progress>
    </header>
    <BaseSidebar>
      <router-view></router-view>
    </BaseSidebar>
    <b-alert
        class="position-fixed fixed-bottom m-4 rounded-0"
        style="z-index: 2000;"
        :variant="$store.state.alert.variant"
        :show="$store.state.alert.countDown"
        @dismissed="$store.state.alert.countDown=0"
        @dismiss-count-down="alertCountdownChanged"
        dismissibe
    >
      {{ $store.state.alert.message }}
    </b-alert>
  </div>
</template>

<script>

import BaseSidebar from "@/components/base/BaseSidebar";

export default {
  name: "Base",
  components: {BaseSidebar},
  created() {
    this.$task.do(
        this.$http.get('/instances').then(x => {
          if (x.data.length === 0) {
            throw Error("PAB responded with empty list of contract instances")
          }
          return x.data.sort((a, b) => a.cicWallet.getWallet - b.cicWallet.getWallet)
        }).then((instances) => {
          const firstInstance = instances[0]
          this.$http.post(
              `instance/${firstInstance.cicContract.unContractInstanceId}/endpoint/funds`, "\"\""
          ).then(
              this.$http.get(
                  `/instance/${firstInstance.cicContract.unContractInstanceId}/status`,
              ).then((x) => {
                this.$store.state.contract.status = x.data.cicCurrentState
                this.$store.state.contract.instances = instances
                this.$store.state.contract.instance = firstInstance

                // Too much Mutations caused Vue.js developer tool crash
                // Reverting back to manual state update for this component
                // this.timeoutHandle = setTimeout(this.refreshStatus, 1500)
                // this.$store.dispatch('updateContractStatus', x.data.cicCurrentState)
                // this.$store.dispatch('updateContractInstances', instances)
                this.refreshStatus()
              }))
        })
    )
  },
  methods: {
    refreshStatus() {
      return this.$http.get(
          `/instance/${this.$store.state.contract.instance.cicContract.unContractInstanceId}/status`,
      ).then((x) => {
        this.$store.state.contract.status = x.data.cicCurrentState
        // this.$store.dispatch('updateContractStatus', x.data.cicCurrentState)
        this.timeoutHandle = setTimeout(this.refreshStatus, 3000)
      })
    },
    alertCountdownChanged(newValue) {
      this.alert.countDown = newValue
    },
    changeProgress: function () {
      if (this.increasing) {
        if (this.loadingProgress > 85) {
          this.increasing = false
          this.loadingProgress += 5;
        } else {
          this.loadingProgress += 12;
        }
      }
      // else {
      //   this.loadingProgress -= 5
      //   if (this.loadingProgress < 70) {
      //     this.increasing = true
      //   } else {
      //     this.loadingProgress -= 5;
      //   }
      // }
    },
    transformFunds(funds) {
      let tokenBalances = []
      let adaBalance = 0
      let index = 0;
      for (let value of funds) {
        if (value.length === 2) {
          let [policy, tokens] = value
          const currency = policy.unCurrencySymbol
          if (currency === "") {
            if (tokens.length === 1 && tokens[0].length === 2) {
              adaBalance = tokens[0][1]
            }
          } else if (currency) {
            for (let token of tokens) {
              if (token.length === 2) {
                if (token[1] > 0) {
                  const tName = token[0].unTokenName
                  tokenBalances.push({
                    index: index,
                    currency: currency,
                    name: tName.startsWith('\u00000x') ? tName.replace('\u00000x', '') : tName,
                    value: token[1]
                  })
                  index++;
                }
              }
            }
          }
        }
      }
      return {
        ada: adaBalance,
        tokens: tokenBalances
      }
    }

  },
  computed: {
    logs() {
      return this.$store.state.contract.status.logs
    },
    currentStatus() {
      const state = this.$store.state.contract.status
      return state ? state.observableState.length : 0
    },
    animate() {
      return this.$store.state.progress.animate
    },
    progressStatus() {
      return this.$store.state.progress.status
    }
  },
  watch: {
    animate(newValue, oldValue) {
      if (newValue) {
        if (!oldValue) {
          this.variant = 'primary'
          this.$refs['progress']._vnode.elm.classList.remove('no-transition')
          this.loadingProgress = 20
          const func = () => {
            this.changeProgress()
            this.timeout = setTimeout(func, 100)
          }
          this.timeout = setTimeout(func, 200)
        }
      } else if (oldValue) {
        if (this.progressStatus) {
          this.variant = 'success'
        }
        this.loadingProgress = 100
        clearTimeout(this.timeout)
        setTimeout(() => {
          this.$refs['progress']._vnode.elm.classList.add('no-transition')
          this.loadingProgress = 0
        }, 700)

      }
    },
    progressStatus(newValue) {
      if (newValue)
        this.variant = 'success';
      else
        this.variant = 'danger';
    },
    logs(newVal, oldVal) {
      if (oldVal !== undefined && newVal.length > oldVal.length) {
        const log = newVal[newVal.length - 1]
        if (log._logLevel === "Error") {
          console.error("PAB Error : "+JSON.stringify(log._logMessageContent,null,2))
          this.$task.errorMessage(log._logMessageContent)
        } else {
          console.log("PAB Log : "+JSON.stringify(log._logMessageContent,null,2))
          this.$task.infoMessage(log._logMessageContent)
        }
      }
    },
    currentStatus(newVal) {
      if (newVal) {
        const last = this.$store.state.contract.status.observableState[newVal - 1]
        if (typeof last === "object" && last.getValue
            && last.getValue.length && last.getValue[0]
            && last.getValue[0].length
            && last.getValue[0][0].unCurrencySymbol !== undefined) {
          console.log("Balance update: \n" + JSON.stringify(last, null, 2))
          // this.$store.dispatch('updateContractFunds', this.transformFunds(last.getValue))
          this.$store.state.contract.funds = this.transformFunds(last.getValue)
        } else {
          console.log("New Api Response: \n" + JSON.stringify(last, null, 2))
          this.$store.state.contract.lastObservable = last
          // this.$store.dispatch('updateContractLastObservable', last)
          if (last.getTxId !== undefined) {
            this.$http.post(
                `instance/${this.$store.state.contract.instance.cicContract.unContractInstanceId}/endpoint/funds`, "\"\""
            )
          }
        }
      }
    }
  },
  destroyed() {
    clearTimeout(this.timeoutHandle)
  },
  data: function () {
    return {
      alert: {
        countDown: 0,
        show: false,
        variant: 'warning',
        message: 'Syncing data..'
      },
      variant: "success",
      loadingProgress: 0,
      timeoutHandle: undefined
    }
  }
}
</script>
<style scoped>
@import "~font-awesome/css/font-awesome.css";

header {
  position: fixed;
  top: 0;
  width: 100vw;
  z-index: 1050;
}

.main-view {
  position: static;
  left: auto;
  z-index: 5;
}

.no-transition {
  -webkit-transition: none !important;
  -moz-transition: none !important;
  -o-transition: none !important;
  -ms-transition: none !important;
  transition: none !important;
}
</style>
