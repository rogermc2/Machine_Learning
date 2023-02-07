
with NL_Types;

package Support_7Aux is

   type XY_Data is record
      Xs : NL_Types.Float_List;
      Ys : NL_Types.Float_List;
   end record;

   function Shoot (Angle : Float) return Float;
   function Show (Angle : Float) return XY_Data;

end Support_7Aux;
