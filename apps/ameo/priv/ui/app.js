//@format
(function() {
  'use strict';
  let ws,
    sn = 0,
    app;

  function initApp() {
    app = new Vue({
      el: '#app',
      methods: {
        clearState: function() {
          this.topics = [];
        },
        pingClicked: function() {
          ping();
        },
        subscribeClicked: function() {
          const topic = this.inputTopic;
          subscribe(topic);
          this.topics.push(topic);
          this.inputTopic = '';
        },
        unsubscribeClicked: function(topic, i) {
          this.topics.splice(i, 1);
          unsubscribe(topic);
        }
      },
      data: {
        inputTopic: '',
        topics: [],
        updates: []
      }
    });
  }

  function init() {
    initApp();
    initWs();
  }

  function log(type, details) {
    app.updates.unshift({
      type: type,
      details: details,
      time: new Date().toLocaleString()
    });
  }

  function initWs() {
    console.log('init!');
    const host = location.host;
    ws = new WebSocket('ws://' + host + '/pubsub/');

    ws.addEventListener('open', function(event) {
      log('connection', 'open');
      console.log('Connection: open', event);
    });

    ws.addEventListener('error', function(event) {
      log('connection', 'error');
      console.warn('Connection: error', event);
    });

    ws.addEventListener('close', function(event) {
      log('connection', 'close');
      app.clearState();
      console.warn('Connection: close', event);
    });

    // Listen for messages
    ws.addEventListener('message', function(event) {
      log('message', event.data);
      console.log('Message: ', JSON.parse(event.data));
    });

    window.setInterval(ping, 15000);
  }

  function send(data) {
    ws.send(JSON.stringify(data));
  }

  function ping() {
    sn += 1;
    send({t: 'ping', sn: sn});
  }

  function subscribe(topic) {
    send({t: 'subunsub', subs: [topic]});
  }

  function unsubscribe(topic) {
    send({t: 'subunsub', unsubs: [topic]});
  }

  window.$ameo = {
    ping: ping,
    subscribe: subscribe,
    unsubscribe: unsubscribe,
    send: send
  };

  window.addEventListener('load', init);
})();
