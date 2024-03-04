<script setup>
  import axios from 'axios';
</script>

<template>
  <main>
    <h1>Беру кредит</h1>

    <div class="container">
      <form v-if="this.step == 0" @submit.prevent="submit">
        <label for="fname">Имя</label>
        <input type="text" v-model="form.first_name" name="firstname">

        <label for="lname">Фамилия</label>
        <input type="text" v-model="form.last_name" name="lastname">

        <label for="email">Email</label>
        <input type="email" v-model="form.email" name="email">

        <label for="salary">Ваша зарплата</label>
        <input type="number" v-model="form.salary" name="salary">

        <label for="amount">Желаемая сумма кредита</label>
        <input type="number" v-model="form.amount" name="amount">

        <button type="submit">Отдать <strike>жопу</strike> душу</button>
      </form>

      <div v-if="this.step == 1">
          <h2 style="margin-bottom: 2rem">Внимательно ознакомьтесь с соглашением!</h2>

          <img style="margin-bottom: 2rem" src="@/assets/tink_ass_sold.png" width="512" height="340" />

          <button @click="sendApplication()">Согласен с условиями</button>
      </div>

    </div>
  </main>
</template>

<script>
export default {
  data() {
    return {
      step: 0,
      form: {
        first_name: '',
        last_name: '',
        email: '',
        salary: 0,
        amount: 0
      }
    }
  },
  methods: {
    async submit() {
      this.$emit('submit', this.form);
      this.step = 1;
    },
    sendApplication() {
      this.step = 0;
      axios.post('http://localhost:8083/api/application', this.form);
    }
  }
}
</script>

<style scoped>
input, select, textarea {
  width: 100%; /* Full width */
  padding: 12px; /* Some padding */
  border: 1px solid hsla(160, 100%, 37%, 1);
  border-radius: 4px; /* Rounded borders */
  box-sizing: border-box; /* Make sure that padding and width stays in place */
  margin-top: 6px; /* Add a top margin */
  margin-bottom: 16px; /* Bottom margin */
  resize: vertical /* Allow the user to vertically resize the textarea (not horizontally) */
}

input:focus {
  outline: none !important;
  border-color: hsla(160, 100%, 37%, 1);
}

/* Style the submit button with a specific background color etc */
button{
  background-color: hsla(160, 100%, 37%, 1);
  color: white;
  padding: 12px 20px;
  border: none;
  border-radius: 4px;
  cursor: pointer;
}

/* When moving the mouse over the submit button, add a darker green color */
button:hover {
  background-color: #45a049;
}

/* Add a background color and some padding around the form */
.container {
  border-radius: 5px;
  padding: 20px;
}
</style>