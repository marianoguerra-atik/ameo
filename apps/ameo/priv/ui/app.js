//@format
(function() {
  'use strict';
  let ws,
    sn = 0;

  function init() {
    console.log('init!');
    const host = location.host;
    ws = new WebSocket('ws://' + host + '/pubsub/');

    ws.addEventListener('open', function(event) {
      console.log('Connection: open', event);
    });

    ws.addEventListener('error', function(event) {
      console.warn('Connection: error', event);
    });

    ws.addEventListener('close', function(event) {
      console.warn('Connection: close', event);
    });

    // Listen for messages
    ws.addEventListener('message', function(event) {
      console.log('Message: ', JSON.parse(event.data));
    });
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
