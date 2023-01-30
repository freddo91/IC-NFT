
module Http = {
    public type HeaderField = (Text, Text);

    public type Token = {};

    public type StreamingCallbackHttpResponse = {
        body : Blob;
        token : Token;
    };

    public type StreamingStrategy = {
        #Callback : {
            callback : shared Token -> async StreamingCallbackHttpResponse;
            token : Token;
        };
    };

    public type HttpRequest = {
        method : Text;
        url : Text;
        headers : [HeaderField];
        body : Blob;
    };

    public type HttpResponse = {
        status_code : Nat16;
        headers : [HeaderField];
        body : Blob;
        streaming_strategy : ?StreamingStrategy;
    };

    public type HttpActor = actor {
        http_request : query (request : HttpRequest) -> async HttpResponse;
        
        http_request_update : shared (request : HttpRequest) -> async HttpResponse;
  };
}