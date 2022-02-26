
package body AR_Types is
   use ML_Types;

   Precision : constant Float := 10.0 ** (-6);

   --  ------------------------------------------------------------------------

   function "=" (L, R : ML_Types.Value_Record) return Boolean is
      Result : Boolean;
   begin
      case L.Value_Kind is
         when Boolean_Type =>
            Result := L.Boolean_Value = R.Boolean_Value;
         when Integer_Type =>
            Result := L.Integer_Value = R.Integer_Value;
         when Float_Type =>
            Result :=
              L.Float_Value >= R.Float_Value - Precision and
              L.Float_Value <= R.Float_Value + Precision;
         when UB_String_Type =>
            Result := L.UB_String_Value = R.UB_String_Value;
      end case;

      return Result;
   end "=";

end AR_Types;
