# Base64 Encode/Decode for Common Lisp

A very simple implementation of [Base64](http://en.wikipedia.org/wiki/Base64) encoding and decoding for Common Lisp.

## Quickstart

Just use the `base64-encode` and `base64-decode` functions to encode and decode from and to strings.

	CL-USER > (base64-encode "Man is distinguished")
	"TWFuIGlzIGRpc3Rpbmd1aXNoZWQ="
	
	CL-USER > (base64-decode *)
	"Man is distinguished"

That's pretty much it. ;-)

In the future, I'll add `base64-encode-binary` and `base64-decode-binary`. But right now I have no need for them. If you create these, please send me a pull request!