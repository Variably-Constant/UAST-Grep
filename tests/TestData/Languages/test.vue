<!--
  Vue Test File for UAST-Grep
  Tests: template syntax, script, styles, composition API, options API
-->
<template>
  <!-- Template section with Vue directives -->
  <div id="app" class="container" :class="{ active: isActive, 'has-error': hasError }">
    <!-- Text interpolation -->
    <h1>{{ title }}</h1>
    <p>{{ message | capitalize }}</p>

    <!-- Attribute binding -->
    <a :href="url" :title="linkTitle" target="_blank">
      {{ linkText }}
    </a>

    <!-- Dynamic attribute -->
    <button :[dynamicAttr]="dynamicValue">Dynamic Attr</button>

    <!-- Conditionals -->
    <div v-if="type === 'A'">Type A content</div>
    <div v-else-if="type === 'B'">Type B content</div>
    <div v-else>Default content</div>

    <p v-show="isVisible">This can be toggled</p>

    <!-- List rendering -->
    <ul>
      <li v-for="(item, index) in items" :key="item.id">
        {{ index }}: {{ item.name }} - {{ item.value }}
      </li>
    </ul>

    <!-- Object iteration -->
    <div v-for="(value, key, index) in object" :key="key">
      {{ index }}. {{ key }}: {{ value }}
    </div>

    <!-- Event handling -->
    <button @click="handleClick">Click me</button>
    <button @click="count++">Increment: {{ count }}</button>
    <button @click.prevent="handleSubmit">Submit</button>
    <input @keyup.enter="onEnter" @keyup.esc="onEscape" />

    <!-- Two-way binding -->
    <input v-model="inputText" placeholder="Type here" />
    <input v-model.lazy="lazyText" />
    <input v-model.number="numericValue" type="number" />
    <input v-model.trim="trimmedText" />

    <textarea v-model="textareaContent"></textarea>

    <select v-model="selected">
      <option disabled value="">Select one</option>
      <option v-for="option in options" :key="option.value" :value="option.value">
        {{ option.text }}
      </option>
    </select>

    <input type="checkbox" v-model="checked" id="checkbox" />
    <label for="checkbox">{{ checked }}</label>

    <!-- Class and style bindings -->
    <div :class="[activeClass, errorClass]">Multiple classes</div>
    <div :style="{ color: activeColor, fontSize: fontSize + 'px' }">Inline style</div>
    <div :style="[baseStyles, overridingStyles]">Multiple styles</div>

    <!-- Slots -->
    <base-layout>
      <template #header>
        <h2>Page Header</h2>
      </template>

      <template #default>
        <p>Main content goes here</p>
      </template>

      <template #footer="{ footerData }">
        <p>Footer: {{ footerData }}</p>
      </template>
    </base-layout>

    <!-- Component usage -->
    <child-component
      :prop-a="valueA"
      :prop-b="valueB"
      @custom-event="handleCustomEvent"
      @update:modelValue="updateValue"
      v-model="componentValue"
      ref="childRef"
    >
      <span>Slot content</span>
    </child-component>

    <!-- Dynamic component -->
    <component :is="currentComponent" v-bind="currentProps" />

    <!-- Transition -->
    <Transition name="fade">
      <p v-if="show">Fading element</p>
    </Transition>

    <!-- TransitionGroup -->
    <TransitionGroup name="list" tag="ul">
      <li v-for="item in listItems" :key="item">
        {{ item }}
      </li>
    </TransitionGroup>

    <!-- Teleport -->
    <Teleport to="body">
      <div v-if="showModal" class="modal">
        <p>Modal content</p>
      </div>
    </Teleport>

    <!-- Suspense -->
    <Suspense>
      <template #default>
        <async-component />
      </template>
      <template #fallback>
        <p>Loading...</p>
      </template>
    </Suspense>

    <!-- Custom directive -->
    <input v-focus />
    <p v-highlight="'yellow'">Highlighted text</p>
  </div>
</template>

<script setup lang="ts">
/**
 * Vue 3 Composition API with script setup
 */
import { ref, reactive, computed, watch, watchEffect, onMounted, onUnmounted } from 'vue';
import type { Ref, ComputedRef } from 'vue';
import ChildComponent from './ChildComponent.vue';

// Type definitions
interface Item {
  id: number;
  name: string;
  value: number;
}

interface Props {
  initialCount?: number;
  items?: Item[];
}

// Props with defaults
const props = withDefaults(defineProps<Props>(), {
  initialCount: 0,
  items: () => [],
});

// Emits
const emit = defineEmits<{
  (e: 'update:count', value: number): void;
  (e: 'custom-event', payload: { id: number; data: string }): void;
}>();

// Reactive references
const count: Ref<number> = ref(props.initialCount);
const inputText = ref('');
const isActive = ref(true);
const hasError = ref(false);
const show = ref(true);

// Reactive object
const state = reactive({
  title: 'UAST-Grep Vue Test',
  message: 'Hello from Vue!',
  type: 'A',
  items: [
    { id: 1, name: 'Item 1', value: 100 },
    { id: 2, name: 'Item 2', value: 200 },
    { id: 3, name: 'Item 3', value: 300 },
  ],
});

// Computed properties
const doubleCount: ComputedRef<number> = computed(() => count.value * 2);

const sortedItems = computed(() => {
  return [...state.items].sort((a, b) => a.value - b.value);
});

// Writable computed
const fullName = computed({
  get() {
    return `${firstName.value} ${lastName.value}`;
  },
  set(value: string) {
    const parts = value.split(' ');
    firstName.value = parts[0] || '';
    lastName.value = parts[1] || '';
  },
});

const firstName = ref('');
const lastName = ref('');

// Watch
watch(count, (newValue, oldValue) => {
  console.log(`Count changed from ${oldValue} to ${newValue}`);
  emit('update:count', newValue);
});

watch(
  () => state.items,
  (newItems) => {
    console.log('Items changed:', newItems);
  },
  { deep: true }
);

// Watch multiple sources
watch([count, inputText], ([newCount, newText], [oldCount, oldText]) => {
  console.log(`Count: ${oldCount} -> ${newCount}, Text: ${oldText} -> ${newText}`);
});

// WatchEffect
watchEffect(() => {
  console.log(`Current count is: ${count.value}`);
});

// Lifecycle hooks
onMounted(() => {
  console.log('Component mounted');
});

onUnmounted(() => {
  console.log('Component unmounted');
});

// Methods
function handleClick(): void {
  count.value++;
}

function handleSubmit(event: Event): void {
  event.preventDefault();
  console.log('Form submitted');
}

async function fetchData(): Promise<Item[]> {
  const response = await fetch('/api/items');
  return response.json();
}

// Expose to parent
defineExpose({
  count,
  increment: handleClick,
});
</script>

<script lang="ts">
/**
 * Options API (for comparison/mixed usage)
 */
import { defineComponent } from 'vue';

export default defineComponent({
  name: 'TestComponent',

  components: {
    ChildComponent,
  },

  props: {
    msg: {
      type: String,
      required: true,
      validator: (value: string) => value.length > 0,
    },
  },

  emits: ['response'],

  data() {
    return {
      optionsApiCount: 0,
    };
  },

  computed: {
    optionsDoubleCount(): number {
      return this.optionsApiCount * 2;
    },
  },

  watch: {
    optionsApiCount(newVal: number): void {
      console.log('Options count:', newVal);
    },
  },

  methods: {
    optionsIncrement(): void {
      this.optionsApiCount++;
    },
  },

  mounted() {
    console.log('Options API mounted');
  },
});
</script>

<style scoped>
/* Scoped styles - only apply to this component */
.container {
  max-width: 1200px;
  margin: 0 auto;
  padding: 20px;
}

.active {
  border: 2px solid green;
}

.has-error {
  border: 2px solid red;
}

button {
  padding: 10px 20px;
  margin: 5px;
  cursor: pointer;
}

/* Deep selector for child components */
:deep(.child-class) {
  color: blue;
}

/* Slotted selector */
:slotted(p) {
  font-style: italic;
}

/* Global selector (escapes scoping) */
:global(.global-class) {
  color: red;
}
</style>

<style lang="scss">
/* SCSS styles */
$primary-color: #3498db;
$secondary-color: #2ecc71;

.container {
  h1 {
    color: $primary-color;
    font-size: 2rem;

    &:hover {
      color: darken($primary-color, 10%);
    }
  }

  .items {
    display: flex;
    gap: 10px;

    .item {
      padding: 10px;
      background: lighten($secondary-color, 30%);

      &.active {
        background: $secondary-color;
      }
    }
  }
}
</style>

<style module>
/* CSS Modules */
.red {
  color: red;
}

.bold {
  font-weight: bold;
}
</style>
