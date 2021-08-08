
package body ML_Types is

    function "<" (L, R : Value_Record) return Boolean is
        Result : Boolean := L.Value_Kind = R.Value_Kind;
    begin
        if Result then
          case L.Value_Kind is
            when Boolean_Type =>
                Result := L.Boolean_Value /= R.Boolean_Value;
            when Integer_Type =>
                Result := L.Integer_Value < R.Integer_Value;
            when Float_Type =>
                Result := L.Float_Value < R.Float_Value;
            when UB_String_Type =>
                Result := L.UB_String_Value < R.UB_String_Value;
          end case;
        end if;

        return Result;
    end "<";

end ML_Types;
