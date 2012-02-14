== HTTP Request Idiom ==

Higher level idiom describing HTTP request and response.

Requests and response are encapsulated in #http_req{} and #http_res{} records.

It differentiate request initiation from request activation to support
HTTP request pipelining.


=== Format Variations ===

http_req
{http_req, list}
{http_req, binary}


=== Specification ===

==== Client Side Messages ===

request_spec()        = {request, Request}
response_spec()       = {response, Request, Response}
body_data_spec()      = {body, Request, Data}
done_spec()           = {done, Request}
error_spec            = {error, Request, Reason}


==== Server Side Messages ===

request_spec()        = {request, Request}
activate_spec()       = {activate, Request}
response_spec()       = {response, Request, Response}
body_data_spec()      = {body, Request, Data}
done_spec()           = {done, Request}
error_spec            = {error, Request, Reason}


==== Common Types ====

Request = #http_req()
Response = #http_res()
Data = string() | binary()
Reason = term()
