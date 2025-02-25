``` ucm
scratch/main> clone @unison/http/releases/3.3.2

  Downloaded 23893 entities.

  Cloned @unison/http/releases/3.3.2.

@unison/http/releases/3.3.2> edit HttpResponse.pattern.statusLine.testReasonIsOptional tests.testFromStream

  ☝️
  
  I added 2 definitions to the top of scratch.u
  
  You can edit them there, then run `update` to replace the
  definitions currently in this namespace.

```
``` unison:added-by-ucm scratch.u
test> HttpResponse.pattern.statusLine.testReasonIsOptional =
  parsed = catch do
    statusNoReason =
      """
      HTTP/1.1 200 
      rest
      """
    IPattern.run statusLine statusNoReason
  verifyAndIgnore do
    assertEquals parsed (Right (Some (["HTTP/1.1", "200", ""], "\r\nrest")))

test> tests.testFromStream = 
  verifyAndIgnore do
    use Path /
    use Text toUtf8
    request =
      """
      GET /docs?%25wei?rd=%26he+llo/&%25wei?rd=+th%23er%3de%25&simple=foo HTTP/1.0
      Content-Type: text/plain
      Content-Length: 12
      
      Hello World!
      """
    stream = do emit (toUtf8 request)
    actual = HttpRequest.fromStream stream
    headers =
      Headers.fromList
        [("Content-Type", "text/plain"), ("Content-Length", "12")]
    expected =
      HttpRequest
        GET
        Version.http10
        (URI
          (Scheme "")
          None
          (root / "docs")
          (RawQuery "%25wei?rd=%26he+llo/&%25wei?rd=+th%23er%3de%25&simple=foo")
          Fragment.empty)
        headers
        (Body (toUtf8 "Hello World!"))
    test.ensureEqual expected actual
```

