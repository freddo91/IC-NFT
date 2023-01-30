/**

 */
import Result "mo:base/Result";
import ExtCore "./Core";
import Principal "mo:base/Principal";
module ExtNonFungible = {
  public type MintRequest = {
    to : ExtCore.User;
    metadata : ?Blob;
  };
  public type Service = actor {
    bearer: query (token : ExtCore.TokenIdentifier) -> async Result.Result<ExtCore.AccountIdentifier, ExtCore.CommonError>;

    mintNFT: shared (request : MintRequest) -> async ();
  };
};
