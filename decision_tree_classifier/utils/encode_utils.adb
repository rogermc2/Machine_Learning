
with Ada.Containers.Ordered_Sets;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Encode_Utils is

    package Bool_Sets is new Ada.Containers.Ordered_Sets (Boolean);
    package Float_Sets is new Ada.Containers.Ordered_Sets (Float);
    package Int_Sets is new Ada.Containers.Ordered_Sets (Integer);
    package UB_String_Sets is new Ada.Containers.Ordered_Sets (Unbounded_String);

    --  -------------------------------------------------------------------------

    function Unique (Values         : ML_Types.Value_Data_List;
                     Inverse        : out Natural_List;
                     Return_Inverse : Boolean := False)
                     return ML_Types.Value_Data_List is
        use ML_Types;
        use Int_Sets;
        use Value_Data_Package;
        use Value_Data_Sorting;
        Values_Curs       : Value_Data_Package.Cursor := Values.First;
        aValue            : Value_Record;
        Bool_Value        : Value_Record (Boolean_Type);
        Float_Value       : Value_Record (Float_Type);
        Int_Value         : Value_Record (Integer_Type);
        UB_String_Value   : Value_Record (UB_String_Type);
        Unique_Booleans   : Bool_Sets.Set;
        Unique_Floats     : Float_Sets.Set;
        Unique_Integers   : Int_Sets.Set;
        Unique_UB_Strings : UB_String_Sets.Set;
        Booleans_Curs     : Bool_Sets.Cursor;
        Floats_Curs       : Float_Sets.Cursor;
        Ints_Curs         : Int_Sets.Cursor;
        UB_Strings_Curs   : UB_String_Sets.Cursor;
        Uniq_List         : Value_Data_List;
    begin
        Inverse.Clear;

        while Has_Element (Values_Curs) loop
            aValue := Element (Values_Curs);
            case aValue.Value_Kind is
            when Boolean_Type =>
                Unique_Booleans.Include (aValue.Boolean_Value);
            when Float_Type =>
                Unique_Floats.Include (aValue.Float_Value);
            when Integer_Type =>
                Unique_Integers.Include (aValue.Integer_Value);
            when UB_String_Type =>
                Unique_UB_Strings.Include (aValue.UB_String_Value);
            end case;
            Next (Values_Curs);
        end loop;

        Booleans_Curs := Unique_Booleans.First;
        while Bool_Sets.Has_Element (Booleans_Curs) loop
            Bool_Value.Boolean_Value := Bool_Sets.Element (Booleans_Curs);
            Uniq_List.Append (Bool_Value);
            Bool_Sets.Next (Booleans_Curs);
        end loop;

        Floats_Curs := Unique_Floats.First;
        while Float_Sets.Has_Element (Floats_Curs) loop
            Float_Value.Float_Value := Float_Sets.Element (Floats_Curs);
            Uniq_List.Append (Float_Value);
            Float_Sets.Next (Floats_Curs);
        end loop;

        Ints_Curs := Unique_Integers.First;
        while Int_Sets.Has_Element (Ints_Curs) loop
            Int_Value.Integer_Value := Int_Sets.Element (Ints_Curs);
            Uniq_List.Append (Int_Value);
            Int_Sets.Next (Ints_Curs);
        end loop;

        UB_Strings_Curs := Unique_UB_Strings.First;
        while UB_String_Sets.Has_Element (UB_Strings_Curs) loop
            UB_String_Value.UB_String_Value :=
              UB_String_Sets.Element (UB_Strings_Curs);
            Uniq_List.Append (UB_String_Value);
            UB_String_Sets.Next (UB_Strings_Curs);
        end loop;

        Sort (Uniq_List);
        if Return_Inverse then
            Values_Curs := Values.First;
            while Has_Element (Values_Curs) loop
                aValue := Element (Values_Curs);
                Inverse.Append (Uniq_List.Find_Index (aValue));
                Next (Values_Curs);
            end loop;
        end if;

        return Uniq_List;

    end Unique;

    -------------------------------------------------------------------------

end Encode_Utils;
