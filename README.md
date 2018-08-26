# plc_interpreter
This is a simple Java/C-like interpreter created for EECS 345 at Case Western Reserve University. 

Example programs this could interpret is
```
class A {

  var x = 100;

  function add(x) {
    return this.x + x;
  }

  static function main() {
    var a = new A();
    return a.add(25);
  }
}
```

```
var x = 14;
var y = 3 * x - 7;
function gcd(a,b) {
  if (a < b) {
    var temp = a;
    a = b;
    b = temp;
  }
  var r = a % b;
  while (r != 0) {
    a = b;
    b = r;
    r = a % b;
  }
  return b;
}
function main () {
  return gcd(x,y);
}
```
