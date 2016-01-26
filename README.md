Main functionality is in src/whiplash_sse_web.erl


### nginx config

    location /sse/ {
        proxy_buffering off;
        proxy_read_timeout 24h;
        proxy_pass http://localhost:4001/;
    }

