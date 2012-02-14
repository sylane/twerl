== HTTP Packets Idiom ==

Idiom of HTTP protocol dialogs.


=== Format Variations ===

http_pkt
{http_pkt, list}
{http_pkt, binary}


=== Specification ===

request_spec()       = {request, Version, Method, Url}
response_spec()      = {response, Version, Status}
                     | {response, Version, StatusCode, StatusMessage}
header_spec()        = {header, HeaderName, HeaderValue}
end_of_header_spec() = eoh
body_data_spec()     = {body, Data}
end_of_body_spec()   = eob
http_error_spec()    = {error, Error}

Version = {VersionHigh, VersionLow}
VersionHigh = VersionLow = int() >= 0
Method = 'GET' | 'HEAD' | 'POST' | 'PUT' | 'DELETE'
Url = #url
Status = StatusCode | atom()
StatusCode = int() > 0
StatusMessage = Data
HeaderName = atom()
HeaderValue = Data
Data = string() | binary()
Error = {bad_request, BadRequestInfo} | {unexpected, Data}
BadRequestInfo = {bad_header, HeaderName, HeaderValue}

