
with Ada.Assertions; use Ada.Assertions;
with Ada.Characters.Handling;
with Ada.Containers;
with Ada.Strings.Fixed;

with Maths;

package body Utilities is

    function Unique_Values (Rows    : Rows_Vector;
                            Feature : Feature_Name_Type) return Value_List;

    --  --------------------------------------------------------------------------

    procedure Check_Rows (Rows : in out Rows_Vector) is
        use Ada.Containers;
        use Rows_Package;
        Data          : Row_Data := Rows.First_Element;
        Num_Features  : constant Class_Range := Data.Class_Count;
        Feature_Types : Data_Type_Array (1 .. Num_Features);
        Label_Type    : Data_Type := Get_Data_Type (Data.Label);
        Data_Changed  : Boolean := False;
    begin
        if Rows.Length < 2 then
            raise Utilities_Exception with
              "Utilities.Check_Rows called with empty rows vector";
        else
            for index in 1 .. Num_Features loop
                Feature_Types (index) := Get_Data_Type (Data.Features (index));
            end loop;

            for row in
              Integer'Succ (Rows.First_Index) .. Rows.Last_Index loop
                Data := Rows.Element (Integer (row));
                Data_Changed := False;
                for col in Class_Range range 1 .. Num_Features loop
                    if Get_Data_Type (Data.Features (col)) = Float_Type and then
                      Feature_Types (col) = Integer_Type then
                        Data.Features (col) := Data.Features (col) & ".0";
                        Feature_Types (col) := Float_Type;
                        Data_Changed := True;
                    elsif
                      Get_Data_Type (Data.Features (col)) = Integer_Type and then
                      Feature_Types (col) = Float_Type then
                        Data.Features (col) := Data.Features (col) & ".0";
                        Data_Changed := True;
                    end if;
                end loop;

                if Get_Data_Type (Data.Label) = Float_Type and then
                  Label_Type = Integer_Type then
                    Data.Label := Data.Label & ".0";
                    Label_Type := Float_Type;
                    Data_Changed := True;
                end if;
                if Data_Changed then
                    Rows.Replace_Element (row, Data);
                end if;
            end loop;
        end if;

    end Check_Rows;

    --  --------------------------------------------------------------------------

    function Get_Column (List_2D      : Value_Data_Lists_2D;
                         Column_Index : Positive)
                     return Value_Data_List is
        aList  : Value_Data_List;
        Column : Value_Data_List;
        Data   : Value_Record;
    begin
        for index in List_2D.First_Index .. List_2D.Last_Index loop
            aList := List_2D.Element (index);
            Data := aList.Element (Column_Index);
            Column.Append (Data);
        end loop;

        return Column;

    end Get_Column;

    --  -------------------------------------------------------------------------

    function Feature_Array (Data    : Rows_Vector;
                            Col_Num : Class_Range) return Value_Data_Array is
        Data_Array : Value_Data_Array (Data.First_Index .. Data.Last_Index);
        UB_Feature : Unbounded_String;
        Data_Kind  : Data_Type;
    begin
        for index in Data.First_Index .. Data.Last_Index loop
            UB_Feature := Data.Element (index).Features (Col_Num);
            Data_Kind := Get_Data_Type (UB_Feature);
            declare
                Feature   : Value_Record (Data_Kind);
                Feature_S : constant String :=
                              To_String (Data.Element (index).Features (Col_Num));
            begin
                case Feature.Value_Kind is
                when Boolean_Type =>
                    Feature.Boolean_Value := Boolean'Value (Feature_S);
                when Float_Type =>
                    Feature.Float_Value := Float'Value (Feature_S);
                when Integer_Type =>
                    Feature.Integer_Value := Integer'Value (Feature_S);
                when UB_String_Type =>
                    Feature.UB_String_Value := To_Unbounded_String (Feature_S);
                end case;
                Data_Array (index) := Feature;
            end; --  declare block
        end loop;
        return Data_Array;
    end Feature_Array;

    --  ---------------------------------------------------------------------------

    function Get_Data_Type (Data : Unbounded_String) return Data_Type is
        theType   : Data_Type;
        aString   : constant String := To_String (Data);
        S_Last    : constant Integer := aString'Last;
        Last_Char : Character;
        UB_Data   : Unbounded_String := Data;
    begin
        Assert (aString'Length > 0,
                "Utilities.Get_Data_Type called with empty string");
        Last_Char := aString (S_Last);
        if Character'Pos (Last_Char) < 32 then
            UB_Data := To_Unbounded_String (aString (1 .. S_Last - 1));
        end if;

        if Is_Integer (UB_Data) then
            theType := Integer_Type;
        elsif Is_Float (UB_Data) then
            theType := Float_Type;
        elsif Is_Boolean (UB_Data) then
            theType := Boolean_Type;
        else
            theType := UB_String_Type;
        end if;

        return theType;

    end Get_Data_Type;

    --  ---------------------------------------------------------------------------

    function Is_Boolean (Item : in Unbounded_String) return Boolean is
        Item_String : constant String :=
                        Ada.Characters.Handling.To_Upper (To_String (Item));
    begin
        return Item_String = "TRUE" or else Item_String = "FALSE";
    end Is_Boolean;

    --  -------------------------------------------------------------------------

    function Is_Float (Item : in Unbounded_String) return Boolean is
        Item_String : constant String := To_String (Item);
        use Ada.Strings;
    begin
        return Fixed.Count (Item_String, ".") = 1;
    end Is_Float;

    --  -------------------------------------------------------------------------

    function Is_Integer (Item : Unbounded_String) return Boolean is
        UB_String : Unbounded_String := Item;
        Dig       : Boolean := True;
    begin
        UB_String := Trim (UB_String, Ada.Strings.Left);
        UB_String := Trim (UB_String, Ada.Strings.Right);

        declare
            Item_String : constant String := To_String (UB_String);
        begin
            for index in Item_String'First .. Item_String'Last loop
                Dig := Dig and then
                  (Ada.Characters.Handling.Is_Decimal_Digit
                     (Item_String (index)) or else
                   Character'Pos (Item_String (index)) < 32);
            end loop;
        end;

        return Dig;

    end Is_Integer;

    --  ---------------------------------------------------------------------------

    function Label_Array (Data : Rows_Vector) return Value_Data_Array is
        Data_Array : Value_Data_Array (Data.First_Index .. Data.Last_Index);
        UB_Label   : Unbounded_String;
        Data_Kind  : Data_Type;
    begin
        for index in Data.First_Index .. Data.Last_Index loop
            UB_Label := Data.Element (index).Label;
            Data_Kind := Get_Data_Type (UB_Label);
            declare
                Label   : Value_Record (Data_Kind);
                Label_S : constant String := To_String (Data.Element (index).Label);
            begin
                case Label.Value_Kind is
                when Boolean_Type =>
                    Label.Boolean_Value := Boolean'Value (Label_S);
                when Float_Type =>
                    Label.Float_Value := Float'Value (Label_S);
                when Integer_Type =>
                    Label.Integer_Value := Integer'Value (Label_S);
                when UB_String_Type =>
                    Label.UB_String_Value := Data.Element (index).Label;
                end case;
                Data_Array (index) := Label;
            end; --  declare block
        end loop;
        return Data_Array;
    end Label_Array;

    --  ---------------------------------------------------------------------------

    function Load_Raw_CSV_Data (Data_File : File_Type)
                            return Raw_Data_Vector is
        use String_Package;
        Data_Line : Unbounded_String;
        CSV_Line  : String_List;
        Curs      : String_Package.Cursor;
        Values    : Unbounded_List;
        Data      : Raw_Data_Vector;
    begin
        while not End_Of_File (Data_File) loop
            Data_Line := To_Unbounded_String (Get_Line (Data_File));
            CSV_Line := Utilities.Split_String
              (To_String (Data_Line), ",");
            Curs := CSV_Line.First;
            Values.Clear;
            while Has_Element (Curs) loop
                Values.Append (Element (Curs));
                Next (Curs);
            end loop;
            Data.Append (Values);
        end loop;

        return Data;

    end Load_Raw_CSV_Data;

    --  -------------------------------------------------------------------------

    function Number_Of_Features (Rows : Rows_Vector) return Class_Range is
        Data  : constant Row_Data := Rows.First_Element;
    begin
        return Data.Class_Count;
    end Number_Of_Features;

    --  -------------------------------------------------------------------------

    function Number_Of_Features (Rows : Value_Data_List) return Class_Range is
    begin
        return Class_Range (Rows.Length);
    end Number_Of_Features;

    --  -------------------------------------------------------------------------

    function Pair_Items (A, B : Float_List) return Float_Pair_List is
        Item   : Float_Pair;
        Result : Float_Pair_List;
    begin
        for index in A.First_Index .. A.Last_Index loop
            Item := (A.Element (index), B.Element (index));
            Result.Append (Item);
        end loop;
        return Result;

    end Pair_Items;

    --  --------------------------------------------------------------------------

    function Pair_Items (a, b : Integer_List) return Integer_Pair_List is
        Item   : Integer_Pair;
        Result : Integer_Pair_List;
    begin
        for index in a.First_Index .. a.Last_Index loop
            Item := (a.Element (index), b.Element (index));
            Result.Append (Item);
        end loop;
        return Result;

    end Pair_Items;

    --  --------------------------------------------------------------------------

    procedure Permute (aList : in out Integer_List) is
        use Integer_Package;
        List_Length  : constant Positive := Positive (aList.Length);
        Curs_1       : Cursor := aList.First;
        Curs_2       : Cursor := aList.First;
        Rand         : Positive;
        Index        : Natural := 0;
        Index_2      : Natural := 0;
    begin
        if List_Length > 1 then
            while Has_Element (Curs_1) loop
                Index := Index + 1;
                Rand := index +
                  Natural (abs (Maths.Random_Float) * Float (List_Length - index));
                Curs_2 := Next (Curs_1);
                Index_2 := 0;
                while Has_Element (Curs_2) and then Index_2 < Rand loop
                    Index_2 := Index_2 + 1;
                    Curs_2 := Next (Curs_2);
                end loop;

                if Has_Element (Curs_2) then
                    Swap (aList, Curs_1, Curs_2);
                end if;
                Next (Curs_1);
            end loop;
        end if;

    end Permute;

    --  -------------------------------------------------------------------------

    procedure Permute (aList : in out String_List) is
        use String_Package;
        List_Length  : constant Positive := Positive (aList.Length);
        Curs_1       : Cursor := aList.First;
        Curs_2       : Cursor := aList.First;
        Rand         : Positive;
        Index        : Natural := 0;
        Index_2      : Natural := 0;
    begin
        if List_Length > 1 then
            while Has_Element (Curs_1) loop
                Index := Index + 1;
                Rand := index +
                  Natural (abs (Maths.Random_Float) * Float (List_Length - index));
                Curs_2 := Next (Curs_1);
                Index_2 := 0;
                while Has_Element (Curs_2) and then Index_2 < Rand loop
                    Index_2 := Index_2 + 1;
                    Curs_2 := Next (Curs_2);
                end loop;

                if Has_Element (Curs_2) then
                    Swap (aList, Curs_1, Curs_2);
                end if;
                Next (Curs_1);
            end loop;
        end if;

    end Permute;

    --  -------------------------------------------------------------------------

    function Permute (aList : Float_List_2D) return Float_List_2D is
        List_Length  : constant Positive := Positive (aList.Length);
        Rand         : Positive;
        Permutation  : Float_List_2D := aList;
    begin
        if List_Length > 1 then
            for index in 1 .. List_Length - 1 loop
                Rand := index +
                  Natural (abs (Maths.Random_Float) * Float (List_Length - index));
                Swap (Permutation, index, Rand);
            end loop;
        end if;

        return Permutation;

    end Permute;

    --  -------------------------------------------------------------------------

    function Permute (aList : Float_List_2D) return Float_List_3D is
        List_Length  : constant Positive := Positive (aList.Length);
        Permutation  : Float_List_2D := aList;
        Permutations : Float_List_3D;

        procedure Recurse (K : Positive; A : in out Float_List_2D) is
        begin
            if K > 1 then
                --  Generate permutations with k-th element unaltered
                Recurse (K - 1, A);
                --  Generate permutations for k-th element swapped with each
                --  k-1 first element
                for index in 1 .. K - 1 loop
                    if K mod 2 = 0 then
                        Swap (A, index, K);
                    else
                        Swap (A, A.First_Index, K);
                    end if;

                    Recurse (K - 1, A);
                end loop;
            else
                Permutations.Append (A);
            end if;

        end Recurse;
        pragma Inline (Recurse);

    begin
        if List_Length > 1 then
            Recurse (List_Length, Permutation);
        else
            Permutations.Append (Permutation);
        end if;

        return Permutations;
    end Permute;

    --  -------------------------------------------------------------------------

    procedure Print_Classification (Classification : Predictions_List) is
        use Prediction_Data_Package;
        Curs        : Cursor := Classification.First;
        Data        : Prediction_Data;
        Predictions : Unbounded_String;
    begin
        Put ("Classification:  {");
        while Has_Element (Curs) loop
            Data := Element (Curs);
            Predictions := Predictions & "'" & To_String (Data.Label) &
              "':" & Natural'Image (Data.Num_Copies);
            if not (Curs = Classification.Last) then
                Predictions := Predictions & ", ";
            end if;
            Next (Curs);
        end loop;
        Predictions := Predictions & "}";
        Put_Line (To_String (Predictions));

    exception
        when others =>
            Put_Line ("Print_Classification exception");
            raise;
    end Print_Classification;

    --  --------------------------------------------------------------------------

    procedure Print_Leaf (Label_Counts : Predictions_List) is
        use Prediction_Data_Package;
        Count_Cursor : Cursor := Label_Counts.First;
        Prediction   : Prediction_Data;
        Total        : Natural := 0;
    begin
        Put_Line ("Predictions:");
        while Has_Element (Count_Cursor) loop
            Total := Total + Element (Count_Cursor).Num_Copies;
            Next (Count_Cursor);
        end loop;

        Count_Cursor := Label_Counts.First;
        while Has_Element (Count_Cursor) loop
            Prediction := Element (Count_Cursor);
            Put_Line  ("{'" & To_String (Prediction.Label) & "': '" &
                         Integer'Image ((100 * Prediction.Num_Copies) / Total) &
                         "%'}");
            Next (Count_Cursor);
        end loop;
    end Print_Leaf;

    --  -------------------------------------------------------------------------

    procedure Print_Value_Record (Message : String; Value : Value_Record) is
        Value_Kind : constant Data_Type := Value.Value_Kind;
    begin
        New_Line;
        Put_Line (Message & " " & Data_Type'Image (Value.Value_Kind) &
                    " value record:");
        Put ("  Value: ");
        case Value_Kind is
        when Integer_Type =>
            Put_Line (Integer'Image (Value.Integer_Value));
        when Float_Type =>
            Put_Line (Float'Image (Value.Float_Value));
        when Boolean_Type =>
            Put_Line (Boolean'Image (Value.Boolean_Value));
        when UB_String_Type => Put_Line (To_String (Value.UB_String_Value));
        end case;

    end Print_Value_Record;

    --  ------------------------------------------------------------------------

    function Prediction_String (Label_Counts : Predictions_List)
                            return String is
        use Prediction_Data_Package;
        Count_Cursor : Cursor := Label_Counts.First;
        Prediction   : Prediction_Data;
        Total        : Natural := 0;
        Leaf_Data    : Unbounded_String := To_Unbounded_String
          ("{'");
    begin
        while Has_Element (Count_Cursor) loop
            Total := Total + Element (Count_Cursor).Num_Copies;
            Next (Count_Cursor);
        end loop;
        Count_Cursor := Label_Counts.First;
        while Has_Element (Count_Cursor) loop
            Prediction := Element (Count_Cursor);
            Leaf_Data := Leaf_Data & To_Unbounded_String
              (To_String (Prediction.Label) & "': '" &
                 Integer'Image ((100 * Prediction.Num_Copies) / Total) &
                 "%'");
            if Count_Cursor /= Label_Counts.Last then
                Leaf_Data := Leaf_Data & ", ";
            end if;
            Next (Count_Cursor);
        end loop;
        return To_String (Leaf_Data) & "}";
    end Prediction_String;

    --  -------------------------------------------------------------------------

    procedure Print_Feature_Values (Message : String; Rows : Rows_Vector;
                                    Column  : Class_Range) is
        use Rows_Package;
        aRow : Row_Data;
    begin
        Put_Line (Message & ":");
        for index in Rows.First_Index .. Rows.Last_Index loop
            aRow := Rows.Element (index);
            Put ("  Feature value: ");
            Put (To_String (aRow.Features (Column)));
            if Column /= aRow.Features'Last then
                Put (", ");
            end if;
            New_Line;
        end loop;

    end Print_Feature_Values;

    --  ------------------------------------------------------------------------

    procedure Print_Feature_Types
      (Message : String; theTypes : Classifier_Utilities.Feature_Type_Array) is
        Count : Natural := 0;
    begin
        Put_Line (Message & " Feature types:");
        for index in theTypes'First .. theTypes'Last loop
            Count := Count + 1;
            Put (Data_Type'Image (theTypes (index)));
            if index /= theTypes'Last then
                Put (", ");
            end if;

            if index /= theTypes'Last and then Count > 5 then
                New_Line;
                Count := 0;
            end if;

        end loop;
        New_Line;

    end Print_Feature_Types;

    --  ------------------------------------------------------------------------

    procedure Print_Label_Types
      (Message : String; theTypes : Classifier_Utilities.Label_Type_Array) is
        Count : Natural := 0;
    begin
        Put_Line (Message & " Label types:");
        for index in theTypes'First .. theTypes'Last loop
            Count := Count + 1;
            Put (Data_Type'Image (theTypes (index)));
            if index /= theTypes'Last then
                Put (", ");
            end if;

            if index /= theTypes'Last and then Count > 5 then
                New_Line;
                Count := 0;
            end if;

        end loop;
        New_Line;

    end Print_Label_Types;

    --  ------------------------------------------------------------------------

    procedure Print_Row (Message : String; aRow : Row_Data) is
    begin
        Put_Line (Message);
        Put ("  Feature values: ");
        for feat in aRow.Features'First .. aRow.Features'Last loop
            Put (To_String (aRow.Features (feat)));
            if feat /= aRow.Features'Last then
                Put (", ");
            end if;
        end loop;
        Put_Line ("; Label: " & To_String (aRow.Label));
    end Print_Row;

    --  ------------------------------------------------------------------------

    procedure Print_Row (Message    : String; Rows : Rows_Vector;
                         Row_Number : Positive) is
        use Rows_Package;
        aRow : Row_Data;
    begin
        Put_Line (Message & ":");
        aRow := Rows.Element (Row_Number);
        Put ("  Feature values: (");
        for feat in aRow.Features'First .. aRow.Features'Last loop
            Put (To_String (aRow.Features (feat)));
            if feat /= aRow.Features'Last then
                Put (", ");
            end if;
        end loop;
        Put_Line ("), Label: " & To_String (aRow.Label));

    end Print_Row;

    --  ------------------------------------------------------------------------

    procedure Print_Rows (Message : String; Rows : Rows_Vector) is
        use Rows_Package;
        aRow : Row_Data;
    begin
        Put_Line (Message & ":");
        for index in Rows.First_Index .. Rows.Last_Index loop
            aRow := Rows.Element (index);
            Put ("  Feature values: (");
            for feat in aRow.Features'First .. aRow.Features'Last loop
                Put (To_String (aRow.Features (feat)));
                if feat /= aRow.Features'Last then
                    Put (", ");
                end if;
            end loop;
            Put_Line ("), Label: " & To_String (aRow.Label));
        end loop;

    end Print_Rows;

    --  ------------------------------------------------------------------------

    procedure Print_Unique_Values (Rows    : Rows_Vector;
                                   Feature : Feature_Name_Type) is
        use Values_Package;
        Values : constant Value_List := Unique_Values (Rows, Feature);
        Curs   : Cursor := Values.First;
        Data   : Value_Data;
    begin
        Put (To_String (Feature) & " Values:");
        while Has_Element (Curs) loop
            Data := Element (Curs);
            case Data.Feature_Kind is
            when Boolean_Type =>
                Put (" " & Boolean'Image (Data.Boolean_Value));
            when Float_Type =>
                Put (" " & Float'Image (Data.Float_Value));
            when Integer_Type =>
                Put (" " & Integer'Image (Data.Integer_Value));
            when UB_String_Type =>
                Put (" " & To_String (Data.UB_String_Value));
            end case;
            Next (Curs);
        end loop;
        New_Line;
    end Print_Unique_Values;

    --  -----------------------------------------------------------------------

    function Split_String (aString, Pattern : String) return String_List is
        use Ada.Strings;
        Last       : constant Integer := aString'Last;
        Last_Char  : constant Character := aString (Last);
        UB_String  : Unbounded_String;
        Split_List : String_List;
    begin
        if Character'Pos (Last_Char) < 32 then
            UB_String :=
              To_Unbounded_String (aString (aString'First .. Last - 1));
        else
            UB_String := To_Unbounded_String (aString);
        end if;

        declare
            String_2 : constant String := To_String (UB_String);
            Last_2   : constant Integer := String_2'Last;
            A_Index  : Integer;
            B_Index  : Integer := String_2'First;
        begin
            for index in String_2'First .. Fixed.Count (String_2, Pattern) loop
                A_Index :=
                  Fixed.Index (String_2 (B_Index .. Last_2), Pattern);
                --  process string slice in any way
                Split_List.Append
                  (To_Unbounded_String (String_2 (B_Index .. A_Index - 1)));
                B_Index := A_Index + Pattern'Length;
            end loop;
            --  process last string
            Split_List.Append
              (To_Unbounded_String (String_2 (B_Index .. Last_2)));
        end;
        return Split_List;

    end Split_String;

    --  -------------------------------------------------------------------------

    procedure Swap (Data : in out Float_List_2D;
                    L, R : Positive) is
        Item : Float_List;
    begin
        Item := Data.Element (L);
        Data.Replace_Element (L, Data.Element (R));
        Data.Replace_Element (R, Item);
    end Swap;

    --  -------------------------------------------------------------------------

    procedure Swap (Data : in out Integer_List;
                    L, R : Positive) is
        Item : Integer;
    begin
        Item := Data.Element (L);
        Data.Replace_Element (L, Data.Element (R));
        Data.Replace_Element (R, Item);
    end Swap;

    --  -------------------------------------------------------------------------

    function Unique_Values (Rows    : Rows_Vector;
                            Feature : Feature_Name_Type) return Value_List is
        use Ada.Containers;
        use Rows_Package;
        use Values_Package;
        Data              : Row_Data := Rows.First_Element;
        Row2              : constant Row_Data :=
                              Rows.Element (Positive'Succ (Rows.First_Index));
        Num_Features      : constant Class_Range := Data.Class_Count;
        Row2_Features     : constant Feature_Data_Array (1 .. Num_Features) :=
                              Row2.Features;
        Feature_Name      : Feature_Name_Type;
        Feature_Data_Type : Data_Type;
        Value_String      : Unbounded_String;
        theSet            : Value_List;

        procedure Add_To_Set (Value : Value_Data) is
        begin
            if not theSet.Contains (Value) then
                theSet.Append (Value);
            end if;
        end Add_To_Set;

    begin
        if Rows.Length < 2 then
            raise Utilities_Exception with
              "Utilities.Unique_Values called with empty rows vector";
        else
            for index in Class_Range range
              Class_Range'Succ (Class_Range (Rows.First_Index)) ..
                Class_Range (Rows.Last_Index) loop
                Data := Rows.Element (Integer (index));
                for col in Class_Range range
                  1 .. Num_Features loop
                    Feature_Name :=
                      Feature_Name_Type (Rows.First_Element.Features (col));
                    if Feature_Name = Feature then
                        Feature_Data_Type :=
                          Get_Data_Type (Row2_Features (col));
                        Value_String := Data.Features (col);
                        case Feature_Data_Type is
                        when Boolean_Type =>
                            declare
                                Feature_Value : Value_Data (Boolean_Type);
                            begin
                                Feature_Value.Feature_Name := Feature;
                                Feature_Value.Boolean_Value :=
                                  Boolean'Value (To_String (Value_String));
                                Add_To_Set (Feature_Value);
                            end;

                        when Float_Type =>
                            declare
                                Feature_Value : Value_Data (Float_Type);
                            begin
                                Feature_Value.Feature_Name := Feature;
                                Feature_Value.Float_Value :=
                                  Float'Value (To_String (Value_String));
                                Add_To_Set (Feature_Value);
                            end;

                        when Integer_Type =>
                            declare
                                Feature_Value : Value_Data (Integer_Type);
                            begin
                                Feature_Value.Feature_Name := Feature;
                                Feature_Value.Integer_Value :=
                                  Integer'Value (To_String (Value_String));
                                Add_To_Set (Feature_Value);
                            end;

                        when UB_String_Type =>
                            declare
                                Feature_Value : Value_Data (UB_String_Type);
                            begin
                                Feature_Value.Feature_Name := Feature;
                                Feature_Value.UB_String_Value := Value_String;
                                Add_To_Set (Feature_Value);
                            end;
                        end case;
                    end if;
                end loop;
            end loop;
        end if;
        return theSet;

    end Unique_Values;

    --  --------------------------------------------------------------------------

    function XY_To_Rows (X, Y : Value_Data_Lists_2D)
                     return Rows_Vector is

        Feature_Values   : Value_Data_List;
        Label_Values     : Value_Data_List;
        aRow             : Row_Data;
        Rows             : Rows_Vector;
    begin
        for index in 1 .. Positive (X.Length) loop
            Feature_Values := X.Element (index);
            Label_Values := Y.Element (index);
            for index2 in Feature_Values.First_Index ..
              Feature_Values.Last_Index loop
                aRow.Features (Class_Range (index)) :=
                  Feature_Values.Element (index2).UB_String_Value;
                aRow.Label :=
                  Label_Values.Element (index2).UB_String_Value;
                Rows.Append (aRow);
            end loop;
        end loop;

        return Rows;

    end XY_To_Rows;

    --  --------------------------------------------------------------------------

end Utilities;
