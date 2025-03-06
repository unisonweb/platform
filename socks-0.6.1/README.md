Socks
=====

Haskell library implementation of the SOCKS 5 protocol.

TODO
----

 * more socks authentification methods: only no authentification is supported for now.
 * support of socks' bind for server to client connection (like FTP).
 * add socks4a and socks4 support.

Usage
-----

See Example.hs for really simple and straighforward example. The main api is only 2 calls:

 * socksConnectAddr which connect to a SockAddr (SockAddrInet or SockAddrInet6).
   The name resolution is left on client side.
 * socksConnectName which connect to a fully qualified domain name "www.example.com".
   The proxy server will do the name resolution.
