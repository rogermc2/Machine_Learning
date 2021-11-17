
package body Export_Types is

   package body Elements is
      function "=" (Left, Right : Element) return Boolean is
         pragma Unreferenced (Right);
      begin
         return False;
      end "=";

      function "<" (Left, Right : Element) return Boolean is
         pragma Unreferenced (Right);
      begin
         return False;
      end "<";
   end Elements;

end Export_Types;
