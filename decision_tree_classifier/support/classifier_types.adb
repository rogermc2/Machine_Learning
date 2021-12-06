
package body Classifier_Types is

   function "-" (L, R : Float_Package.Vector) return Float_Package.Vector is
      Result : Float_Package.Vector;
   begin
      for index in L.First_Index .. L.Last_Index loop
         Result.Append (L.Element (index) - R.Element (index));
      end loop;

      return Result;

   end "-";

   --  ----------------------------------------------------------------------------

   function "abs" (aVector : Float_Package.Vector) return Float_Package.Vector is
      Result : Float_Package.Vector;
   begin
      for index in aVector.First_Index .. aVector.Last_Index loop
         Result.Append (abs (aVector.Element (index)));
      end loop;

      return Result;

   end "abs";

   --  ----------------------------------------------------------------------------

   function Dot (L, R : Float_Package.Vector) return Float is
      Result : Float := 0.0;
   begin
      for index in L.First_Index .. L.Last_Index loop
         Result := Result + L.Element (index) * R.Element (index);
      end loop;

      return Result;

   end Dot;

   --  ----------------------------------------------------------------------------

end Classifier_Types;
