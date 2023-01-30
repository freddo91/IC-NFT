import Nat32 "mo:base/Nat32";
import Nat "mo:base/Nat";
import Text "mo:base/Text";
import Hash "mo:base/Hash";
import HashMap "mo:base/HashMap";
import Iter "mo:base/Iter";
import Char "mo:base/Char";
import Array "mo:base/Array";

module {
    public func nat32FromText(t : Text) : Nat32 {
        var map = HashMap.HashMap<Nat, Nat32>(1, Nat.equal, Hash.hash);
        // '0': 48 -> 0; '9': 57 -> 9
        for (num in Iter.range(48, 57)) {
            map.put(num, Nat32.fromNat(num-48));
        };
        let p = Iter.toArray(Iter.map(Text.toIter(t), func (x: Char) : Nat { Nat32.toNat(Char.toNat32(x)) }));
        var sum: Nat32 = 0; 
        for (i in p.vals()) {
            let val = switch (map.get(i)) {
                case (?n) n;
                case null return 0;
            };       
            sum *= 10;
            sum += val;
        };
        return sum;
    };

    public func textToNat32(t : Text) : Nat32 {
        var reversed : [Nat32] = [];
        for(c in t.chars()) {
            assert(Char.isDigit(c));
            reversed := Array.append([Char.toNat32(c)-48], reversed);
        };
        var total : Nat32 = 0;
        var place : Nat32  = 1;
        for(v in reversed.vals()) {
            total += (v * place);
            place := place * 10;
        };
        total;
    };

    public func getParam(url : Text, param : Text) : ?Text {
        var _s : Text = url;
        Iter.iterate<Text>(Text.split(_s, #text("/")), func(x, _i) {
            _s := x;
        });
        Iter.iterate<Text>(Text.split(_s, #text("?")), func(x, _i) {
            if (_i == 1) _s := x;
        });
        var t : ?Text = null;
        var found : Bool = false;
        Iter.iterate<Text>(Text.split(_s, #text("&")), func(x, _i) {
            if (found == false) {
                Iter.iterate<Text>(Text.split(x, #text("=")), func(y, _ii) {
                    if (_ii == 0) {
                        if (Text.equal(y, param)) found := true;
                    } else if (found == true) t := ?y;
                });
            };
        });
        return t;
    };
};