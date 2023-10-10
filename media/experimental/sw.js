// service worker for the experimental version (changes cacheName)

var cacheName = 'funcially-pwa-experimental';
var filesToCache = [
  './',
  './index.html',
  './gui.js',
  './gui_bg.wasm',
];

/* Start the service worker and cache all of the app's content */
self.addEventListener('install', function (e) {
  e.waitUntil(
    caches.open(cacheName).then(function (cache) {
      return cache.addAll(filesToCache);
    })
  );
});

/* Serve cached content when offline */
self.addEventListener('fetch', function (e) {
  // "Stale while revalidate" approach

  e.respondWith(
    caches.match(e.request).then(cachedResponse => {
      const networkFetch = fetch(e.request).then(response => {
        // update the cache
        caches.open(cacheName).then(cache => {
          cache.put(e.request, response.clone());
        });
      });

      // prioritize cached response over network
      return cachedResponse || networkFetch;
    })
  );
});