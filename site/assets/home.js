const floatingShapes = [
  { emoji: '📊', left: '10%', delay: '0s' },
  { emoji: '🎲', left: '30%', delay: '3s' },
  { emoji: '📈', left: '50%', delay: '6s' },
  { emoji: '🃏', left: '70%', delay: '9s' },
  { emoji: '⚡', left: '90%', delay: '12s' }
];

const navItems = [
  { name: 'Tools', icon: '🎯', route: 'tools' },
  { name: 'Courses', icon: '📚', route: 'courses' },
  { name: 'Activities', icon: '🎮', route: 'activities' },
  { name: 'Lessons', icon: '📖', route: 'lessons' },
  { name: 'Profile', icon: '👤', route: 'profile' }
];

const stats = [
  { icon: '📊', label: 'Visualize Data' },
  { icon: '🎯', label: 'Interactive Learning' },
  { icon: '⚡', label: 'Real-time Analysis' },
  { icon: '🧠', label: 'Smart Insights' }
];

const apps = [
  {
    id: 'dice',
    path: '/dice/',
    class: 'dice-card',
    icon: '🎲',
    title: 'Dice Roll Simulator',
    description: 'Explore probability fundamentals with interactive dice rolling. Analyze patterns, understand distributions, and visualize statistical concepts.',
    demoText: null,
    buttonText: 'Roll'
  },
  {
    id: 'cards',
    path: '/example/',
    class: 'cards-card',
    icon: '🃏',
    title: 'Random Card Generator',
    description: 'Draw random playing cards and explore combinatorics. Perfect for understanding permutations, combinations, and conditional probability.',
    demoText: null,
    buttonText: 'Draw'
  },
  {
    id: 'distributions',
    path: '/example/',
    class: 'distributions-card',
    icon: '📊',
    title: 'Probability Distributions',
    description: 'Visualize and interact with various probability distributions. Understand normal, binomial, Poisson, and other key distributions.',
    demoText: 'Normal Distribution: μ=0, σ=1',
    buttonText: 'Explore'
  },
  {
    id: 'hypothesis',
    path: '/example/',
    class: 'hypothesis-card',
    icon: '🔬',
    title: 'Hypothesis Testing',
    description: 'Learn statistical inference through interactive hypothesis tests. Understand p-values, significance levels, and statistical power.',
    demoText: 't-test, z-test, chi-square',
    buttonText: 'Test'
  },
  {
    id: 'regression',
    path: '/example/',
    class: 'regression-card',
    icon: '📈',
    title: 'Regression Analysis',
    description: 'Explore relationships between variables with interactive regression tools. Linear, multiple, and logistic regression made visual.',
    demoText: 'R² = 0.85, p < 0.001',
    buttonText: 'Analyze'
  },
  {
    id: 'timeseries',
    path: '/example/',
    class: 'timeseries-card',
    icon: '⏱️',
    title: 'Time Series Analysis',
    description: 'Analyze temporal data patterns with interactive time series tools. Forecasting, trend analysis, and seasonality detection.',
    demoText: 'ARIMA, Seasonal Decomposition',
    buttonText: 'Forecast'
  }
];

const { createApp } = Vue;

createApp({
  data () {
    return {
      currentRoute: 'home',
      notification: null,

      // Floating shapes
      floatingShapes,

      // Navigation items
      navItems,

      // Hero stats
      stats,

      // Features
      features: apps,

      // Demo dice state
      demoDice: ['⚃', '⚁'],
      isRollingDice: false,

      // Demo card state
      demoCard: {
        value: 'K',
        suit: '♥',
        suitClass: 'red-suit'
      },
      isFlippingCard: false
    }
  },

  mounted () {
    this.startParticleGeneration();
  },

  methods: {
    navigateTo (route) {
      this.currentRoute = route;
      this.showNotification(`Navigating to ${route}...`);
    },

    navigateToFeature (featureId) {
      this.currentRoute = featureId;
      const feature = this.features.find(f => f.id === featureId);
      this.showNotification(`Opening ${feature.title}...`);
      window.location.href = `${feature.path}`;
    },

    showNotification (message) {
      this.notification = message;
      setTimeout(() => {
        this.notification = null;
      }, 3000);
    },

    rollDemoDice () {
      const faces = ['⚀', '⚁', '⚂', '⚃', '⚄', '⚅'];
      this.isRollingDice = true;

      setTimeout(() => {
        this.demoDice = [
          faces[Math.floor(Math.random() * 6)],
          faces[Math.floor(Math.random() * 6)]
        ];
        this.isRollingDice = false;
      }, 600);
    },

    drawDemoCard () {
      const values = ['A', '2', '3', '4', '5', '6', '7', '8', '9', '10', 'J', 'Q', 'K'];
      const suits = [
        { symbol: '♠', class: '' },
        { symbol: '♥', class: 'red-suit' },
        { symbol: '♦', class: 'red-suit' },
        { symbol: '♣', class: '' }
      ];

      this.isFlippingCard = true;

      setTimeout(() => {
        const randomValue = values[Math.floor(Math.random() * values.length)];
        const randomSuit = suits[Math.floor(Math.random() * suits.length)];

        this.demoCard = {
          value: randomValue,
          suit: randomSuit.symbol,
          suitClass: randomSuit.class
        };
        this.isFlippingCard = false;
      }, 400);
    },

    startParticleGeneration () {
      setInterval(() => {
        this.createParticle();
      }, 500);
    },

    createParticle () {
      const particle = document.createElement('div');
      particle.style.position = 'fixed';
      particle.style.width = '3px';
      particle.style.height = '3px';
      particle.style.background = 'rgba(74, 144, 226, 0.6)';
      particle.style.borderRadius = '50%';
      particle.style.pointerEvents = 'none';
      particle.style.left = Math.random() * window.innerWidth + 'px';
      particle.style.top = window.innerHeight + 'px';
      particle.style.zIndex = '1';

      document.body.appendChild(particle);

      const duration = 4000 + Math.random() * 2000;
      particle.animate([
        { transform: 'translateY(0px)', opacity: 1 },
        { transform: `translateY(-${window.innerHeight + 100}px)`, opacity: 0 }
      ], {
        duration: duration,
        easing: 'linear'
      }).onfinish = () => particle.remove();
    }
  }
}).mount('#app');
