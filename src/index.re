[%bs.raw {|require('./index.css')|}];

[@bs.module "./registerServiceWorker"] external register_service_worker : unit => unit = "default";

open App;

ReactDOMRe.renderToElementWithId(<App />, "root");

register_service_worker();
