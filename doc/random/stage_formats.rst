== Stage Format Secifications ==

Here are specified the basic formats, every idioms define there own.
See idiom definitions in doc/idioms for more format specifications.


=== Producer Inputs ===

any

none

query

message


=== Consumer Outputs ===

any

none


=== Others ===

any

binary
{binary, line}
{binary, field}
{binary, {field, Sep}}         when is_list(Sep); is_binary(Sep)
{binary, {packet, Header}}     when Header =:= 1; Header =:= 2; Header =:= 4
{binary, {packet, Format}}     when Format =:= asn1; Format =:= cdr;
                                    Format =:= sunrm; Format =:= fcgi;
                                    Format =:= tpkt
{binary, block}
{binary, {block, MaxSize}}     when is_intenger(MaxSize)

list
{list, line}
{list, field}
{list, {field, Sep}}           when is_list(Sep); is_binary(Sep)
{list, {packet, Header}}       when Header =:= 1; Header =:= 2; Header =:= 4
{list, {packet, Format}}       when Format =:= asn1; Format =:= cdr;
                                    Format =:= sunrm; Format =:= fcgi;
                                    Format =:= tpkt
{list, block}
{list, {block, MaxSize}}       when is_intenger(MaxSize)

iodata
{iodata, line}
{iodata, field}
{iodata, {field, Sep}}         when is_list(Sep); is_binary(Sep)
{iodata, {packet, Header}}     when Header =:= 1; Header =:= 2; Header =:= 4
{iodata, block}
{iodata, {block, MaxSize}}     when is_integer(MaxSize)

# Standard erlang HTTP packets
http
{http, binary}
{http, list}

banana
{banana, binary}
{banana, list}


=== Canonical Forms ===

binary -> {binary, block}
list -> {list, block}
iodata -> {iodata, block}
