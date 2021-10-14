
with Ada.Containers.Ordered_Sets;
with Ada.Text_IO; use Ada.Text_IO;

with Base_Decision_Tree;
with Encode_Utils;

package body Classifier_Utilities is

    package Int_Sets is new Ada.Containers.Ordered_Sets (Integer);

    --  -------------------------------------------------------------------------
    --  Bin_Count counts the number of occurrences of each value in Numbers.
    --  Each bin gives the number of occurrences of its index value in Numbers.
    function Bin_Count (Numbers : Natural_List) return Natural_List is
        use Ada.Containers;
        use Natural_Package;
        aNumber    : Natural := 0;
        Max_Number : Natural := 0;
        Bins       : Natural_List;
    begin
        for index in Numbers.First_Index .. Numbers.Last_Index loop
            aNumber := Numbers.Element (index);
            if aNumber > Max_Number then
                Max_Number := aNumber;
            end if;
        end loop;

        Bins.Set_Length (Count_Type (Max_Number + 1));
        for index in Bins.First_Index .. Bins.Last_Index loop
            Bins (index) := 0;
        end loop;

        for index in Numbers.First_Index .. Numbers.Last_Index loop
            aNumber := Numbers.Element (index);
            Bins (aNumber + 1) := Bins (aNumber + 1) + 1;
        end loop;

        return Bins;

    end Bin_Count;

    --  -------------------------------------------------------------------------

    function Bin_Count (Numbers : ML_Types.Value_Data_List)
                        return Natural_List is
        use Ada.Containers;
        use ML_Types;
        use Natural_Package;
        aNumber    : Natural := 0;
        Max_Number : Natural := 0;
        Bins       : Natural_List;
    begin
        for index in Numbers.First_Index .. Numbers.Last_Index loop
            if Numbers.Element (index).Value_Kind /= Integer_Type then
                raise Value_Error with
                  "Classifier_Utilities.Bin_Count called with non-integer data "
                  & Data_Type'Image (Numbers.Element (index).Value_Kind);
            end if;

            aNumber := Numbers.Element (index).Integer_Value;
            if aNumber > Max_Number then
                Max_Number := aNumber;
            end if;
        end loop;

        Bins.Set_Length (Count_Type (Max_Number + 1));
        for index in Bins.First_Index .. Bins.Last_Index loop
            Bins (index) := 0;
        end loop;

        for index in Numbers.First_Index .. Numbers.Last_Index loop
            aNumber := Numbers.Element (index).Integer_Value;
            Bins (aNumber + 1) := Bins (aNumber + 1) + 1;
        end loop;

        return Bins;

    end Bin_Count;

    --  -------------------------------------------------------------------------

    procedure Clear (anArray : in out ML_Types.Value_Data_Array) is
    begin
        for index in anArray'Range loop
            anArray (index).Float_Value := 0.0;
        end loop;
    end Clear;

    --  -------------------------------------------------------------------------

    function Compare_Float_Lists (L, R : Float_List) return Boolean is
        use Ada.Containers;
        use Float_Package;
        Diff     : Float := 0.0;
        Max_Diff : Float := 0.0;
        OK       : Boolean := R.Length = L.Length;
    begin
        if OK then
            for index in L.First_Index .. L.Last_Index loop
                Diff := Abs (R.Element (index) - L.Element (index));
                OK := OK and (Diff < 1.5 * 10.0 ** (-6));
                if Diff > Max_Diff then
                    Max_Diff := Diff;
                end if;
            end loop;

            if not OK then
                Put ("Classifier_Utilities.Compare_Float_Lists test ");
                Put_Line ("failed with error: " & Float'Image (Max_Diff));
            end if;
        else
            Put ("Classifier_Utilities.Compare_Float_Lists ");
            Put_Line ("test failed with different length lists, Left: "
                      & Count_Type'Image (L.Length) & ", Right: " &
                        Count_Type'Image (R.Length));
        end if;

        return OK;
    end Compare_Float_Lists;

    --  -------------------------------------------------------------------------

    function Dot (L : Weights.Weight_List;
                  R : Natural_List) return Float is
        use Float_Package;
        Result : Float := 0.0;
    begin
        for index in L.First_Index .. L.Last_Index loop
            Result := Result + L.Element (index) * Float (R.Element (index));
        end loop;

        return Result;

    end Dot;

    --  -------------------------------------------------------------------------

    function Search_Sorted_Value_List (List_A, List_B : ML_Types.Value_Data_List)
                                       return Integer_List is
        use ML_Types;
        use Integer_Package;
        use Value_Data_Package;
        use Value_Data_Sorting;
        Item    : Value_Record;
        Index_A : Positive;
        theList : Integer_List;
    begin
        if not Is_Sorted (List_A) then
            raise Value_Error with
              "Search_Sorted called with unsorted list.";
        end if;

        for index_B in List_B.First_Index .. List_B.Last_Index loop
            Item := List_B.Element (index_B);
            Index_A := List_A.Find_Index (Item);
            if Index_A /= Value_Data_Package.No_Index then
                theList.Append (Index_A);
            end if;
        end loop;
        return theList;
    end Search_Sorted_Value_List;

    --  -------------------------------------------------------------------------

    function To_Array (L : Integer_List) return Integer_Array is
        New_Array : Integer_Array (1 .. Integer (L.Length));
        A_Index   : Integer := 0;
    begin
        for index in L.First_Index .. L.Last_Index loop
            A_Index := A_Index + 1;
            New_Array (A_Index) := L.Element (index);
        end loop;
        return New_Array;
    end To_Array;

    --  -------------------------------------------------------------------------

    function To_Float_List (A : Float_Array) return Float_List is
        A_List : Float_List;
    begin
        for index in A'Range loop
            A_List.Append (A (index));
        end loop;
        return A_List;
    end To_Float_List;

    --  -------------------------------------------------------------------------

    function To_Integer_List (A : Integer_Array) return Integer_List is
        A_List : Integer_List;
    begin
        for index in A'Range loop
            A_List.Append (A (index));
        end loop;
        return A_List;

    end To_Integer_List;

    --  -------------------------------------------------------------------------

    function To_Integer_Value_List (A : Integer_Array)
                                    return ML_Types.List_Of_Value_Data_Lists is
        use ML_Types;
        Data       : Value_Record (Integer_Type);
        B_List     : Value_Data_List;
        Multi_List : List_Of_Value_Data_Lists;
    begin
        for index in A'Range loop
            B_List.Clear;
            Data.Integer_Value := A (index);
            B_List.Append (Data);
            Multi_List.Append (B_List);
        end loop;

        return Multi_List;
    end To_Integer_Value_List;

    --  -------------------------------------------------------------------------

    function To_Multi_Value_List (A : Multi_Value_Array)
                                  return ML_Types.List_Of_Value_Data_Lists is
        use ML_Types;
        Value    : Value_Record (Integer_Type);
        Row_List : List_Of_Value_Data_Lists;
        Col_List : Value_Data_List;
    begin
        for row in A'Range loop
            Col_List.Clear;
            for col in A'Range (2) loop
                Value.Integer_Value := A (row, col);
                Col_List.Append (Value);
            end loop;
            Row_List.Append (Col_List);
        end loop;
        return Row_List;

    end To_Multi_Value_List;

    --  -------------------------------------------------------------------------

    function To_Natural_List (A : Natural_Array) return Natural_List is
        A_List : Natural_List;
    begin
        for index in A'Range loop
            A_List.Append (A (index));
        end loop;
        return A_List;

    end To_Natural_List;

    --  -------------------------------------------------------------------------

    function To_Natural_Value_List (A : Natural_Array)
                                    return ML_Types.List_Of_Value_Data_Lists is
        Int_Array : Integer_Array (1 .. A'Length);
    begin
        for index in A'Range loop
            Int_Array (index) := A (index);
        end loop;
        return To_Integer_Value_List (Int_Array);
    end To_Natural_Value_List;

    --  -------------------------------------------------------------------------

    procedure Print_Boolean_Matrix (Name    : String;
                                    aMatrix : Estimator.Boolean_Matrix) is
    begin
        Put_Line (Name & ": ");
        for row in aMatrix'First .. aMatrix'Last loop
            for col in aMatrix'First (2)  .. aMatrix'Last (2) loop
                Put (Boolean'Image (aMatrix (row, col)) & "  ");
            end loop;
            New_Line;
        end loop;
        New_Line;
    end Print_Boolean_Matrix;

    --  ------------------------------------------------------------------------

    generic
        type Index_Type is (<>);
        type Vector_Type is  array (Index_Type) of aliased Float;
    procedure Print_Floats_Vector (Name : String; aVector : Vector_Type);

    procedure Print_Floats_Vector (Name : String; aVector : Vector_Type) is
    begin
        if Name = "" then
            Put ("  ");
        else
            Put (Name & ":  ");
        end if;
        for Index in aVector'Range loop
            Put (Float'Image (aVector (Index)) & "   ");
        end loop;
        New_Line;
    end Print_Floats_Vector;

    --  -------------------------------------------------------------------

    generic
        type Index_Type is (<>);
        type Vector_Type is  array (Index_Type) of aliased Integer;
    procedure Print_Integer_Vector (Name : String; aVector : Vector_Type);

    --  -------------------------------------------------------------------

    procedure Print_Integer_Vector (Name : String; aVector : Vector_Type) is
    begin
        if Name = "" then
            Put ("  ");
        else
            Put (Name & ":  ");
        end if;
        for Index in aVector'Range loop
            Put (Integer'Image (aVector (Index)) & "   ");
        end loop;
        New_Line;
    end Print_Integer_Vector;

    --  -------------------------------------------------------------------

    procedure Print_Float_Array (Name          : String; anArray : Float_Array;
                                 Start, Finish : Integer) is
        Count : Integer := 1;
    begin
        Put_Line (Name & ": ");
        if Start >= anArray'First and then Finish <= anArray'Last then
            for Index in Start .. Finish loop
                Put (Float'Image (anArray (Index)) & "  ");
                Count := Count + 1;
                if Count > 4 then
                    New_Line;
                    Count := 1;
                end if;
            end loop;
        else
            Put_Line ("Print_Float_Array called with invalid start or finish index.");
        end if;
        New_Line;
    end Print_Float_Array;

    --  ------------------------------------------------------------------------

    procedure Print_Float_List (Name : String; theList : Float_List) is
        Count : Integer := 1;
    begin
        Put_Line (Name & ": ");
        for Index in theList.First_Index .. theList.Last_Index loop
            Put (Integer'Image (Index) & ": " &
                   Float'Image (theList.Element (Index)) & "   ");
            Count := Count + 1;
            if Count > 4 then
                New_Line;
                Count := 1;
            end if;
        end loop;
    end Print_Float_List;

    --  ------------------------------------------------------------------------

    procedure Print_Integer_Array (Name : String; anArray : Integer_Array) is
    begin
        Put_Line (Name & ": ");
        for Index in anArray'First .. anArray'Last loop
            Put_Line (Integer'Image (Index) & ":  " &
                        Integer'Image (anArray (Index)));
        end loop;
        New_Line;
    end Print_Integer_Array;

    --  ------------------------------------------------------------------------

    procedure Print_Multi_Value_Array (Name    : String;
                                       anArray : Multi_Value_Array) is
    begin
        Put (Name);
        if anArray'Length > 0 and anArray'First > 0 then
            Put_Line (": ");
            for Index in anArray'First .. anArray'Last loop
                Put_Line (Integer'Image (anArray (Index, 1)) & ",  " &
                            Integer'Image (anArray (Index, 2)));
            end loop;
        elsif anArray'Length = 0 then
            Put_Line (" is empty.");
        else
            raise Value_Error with
              "Print_Multi_Value_Array called with invalid index: " &
              Integer'Image (Integer (anArray'First));
        end if;

        New_Line;
    end Print_Multi_Value_Array;

    --  ------------------------------------------------------------------------

    procedure Print_Integer_List (Name : String; theList : Integer_List) is
        Count : Integer := 1;
    begin
        Put_Line (Name & ": ");
        for Index in theList.First_Index .. theList.Last_Index loop
            Put (Integer'Image (theList.Element (Index)) & "   ");
            Count := Count + 1;
            if Count > 10 then
                New_Line;
                Count := 1;
            end if;
        end loop;
        New_Line;
    end Print_Integer_List;

    --  ------------------------------------------------------------------------

    procedure Print_List_Of_Float_Lists (Name : String;
                                         Data : List_Of_Float_Lists) is
    begin
        Put_Line (Name & ": ");
        for index in Data.First_Index .. Data.Last_Index loop
            Print_Float_List ("List" & Integer'Image (index) & ": ",
                              Data.Element (index));
        end loop;
        New_Line;

    end Print_List_Of_Float_Lists;

    --  ------------------------------------------------------------------------

    procedure Print_List_Of_Natural_Lists (Name : String;
                                           Data : List_Of_Natural_Lists) is
    begin
        Put_Line (Name & ": ");
        for Index in Data.First_Index .. Data.Last_Index loop
            Print_Natural_List ("", Data.Element (Index));
        end loop;
        New_Line;

    end Print_List_Of_Natural_Lists;

    --  ------------------------------------------------------------------------

    procedure Print_List_Of_Value_Lists
      (Name : String; Multi_List : Tree.List_Of_Values_Lists) is
    begin
        Print_List_Of_Float_Lists (Name, Multi_List);
    end Print_List_Of_Value_Lists;

    --  ------------------------------------------------------------------------

    procedure Print_List_Of_Value_Data_Lists
      (Name : String; Multi_List : ML_Types.List_Of_Value_Data_Lists) is
    begin
        Put_Line (Name & ": ");
        for index in Multi_List.First_Index .. Multi_List.Last_Index loop
            Print_Value_Data_List ("List" & Integer'Image (index),
                                   Multi_List.Element (index));
        end loop;
        New_Line;
    end Print_List_Of_Value_Data_Lists;

    --  ------------------------------------------------------------------------

    procedure Print_Natural_List (Name : String; theList : Natural_List) is
        Count : Integer := 1;
    begin
        if Name'Length > 0 then
            Put (Name & ": ");
        end if;

        for Index in theList.First_Index .. theList.Last_Index loop
            Put (Natural'Image (theList.Element (Index)) & "   ");
            Count := Count + 1;
            if Count > 10 then
                New_Line;
                Count := 1;
            end if;
        end loop;
        New_Line;
    end Print_Natural_List;

    --  ------------------------------------------------------------------------

    procedure Print_Node (Node : Tree.Tree_Node; Offset : String := "") is
        UB_Offset   : constant Unbounded_String :=
                        To_Unbounded_String (Offset);
    begin
        Put (To_String (UB_Offset & "Type of node: "));
        if Node.Is_Leaf then
            Put_Line ("Leaf");
        else
            Put_Line ("Decision");
        end if;
        Put_Line (To_String (UB_Offset & "Number of weighted samples:" &
                    Integer'Image (Node.Weighted_Num_Node_Samples)));
        Put_Line (To_String (UB_Offset & "Start:" &
                    Integer'Image (Node.Samples_Start)));
        Put_Line (To_String (UB_Offset & "Number of samples:" &
                    Integer'Image (Node.Num_Node_Samples)));
        if Node.Is_Left then
            Put_Line (To_String (UB_Offset & "True branch"));
        else
            Put_Line (To_String (UB_Offset & "False branch"));
        end if;
        Put_Line (To_String (UB_Offset & "Number of constant features:" &
                    Integer'Image (Node.Num_Constant_Features)));
        Put_Line (To_String (UB_Offset & "Depth:" &
                    Integer'Image (Node.Depth)));
        Put_Line (To_String (UB_Offset & "Impurity: " &
                    Float'Image (Node.Impurity)));

        if Node.Is_Leaf then
            Print_List_Of_Value_Lists (To_String (UB_Offset & "Prediction: "),
                                       Node.Values);
        else
            Put_Line (To_String (UB_Offset & "Feature index: " &
                        Integer'Image (Node.Feature_Index)));
            Put_Line (To_String (UB_Offset & "Threshold: " &
                        Float'Image (Node.Threshold)));
        end if;

    end Print_Node;

    --  ------------------------------------------------------------------------

    procedure Print_Node (Message : String; Node : Tree.Tree_Node) is
    begin
        New_Line;
        Put_Line (Message);
        Print_Node (node);
        New_Line;
    end Print_Node;

    --  ------------------------------------------------------------------------

    procedure Print_Split_Record (Name : String;
                                  Data : Node_Splitter.Split_Record) is
    begin
        Put_Line (Name & ": ");
        Put_Line ("Feature_Index:  " & Integer'image (Data.Feature));
        Put_Line ("Position:       " & Integer'image (Data.Pos_I));
        Put_Line ("Threshold:      " & Float'image (Data.Threshold));
        Put_Line ("Improvement:    " & Float'image (Data.Improvement));
        Put_Line ("Impurity_Left:  " & Float'image (Data.Impurity_Left));
        Put_Line ("Impurity_Right: " & Float'image (Data.Impurity_Right));
        New_Line;

    end Print_Split_Record;

    --  ------------------------------------------------------------------------

    procedure Print_Tree (Name  : String;
                          aTree : Base_Decision_Tree.Classifier) is
        Tree_Nodes  : constant Tree.Tree_Class :=
                        aTree.Attributes.Decision_Tree;
    begin
        Print_Tree (Name, Tree_Nodes);
    end Print_Tree;

    --  -------------------------------------------------------------------------

    procedure Print_Tree (Name  : String;
                          aTree : Tree.Tree_Class) is
        use Tree;
        use Nodes_Package;
        use Float_Package;
        Tree_Nodes  : constant Tree.Tree_Nodes := aTree.Nodes;
--          Values      : constant List_Of_Float_Lists := aTree.Values;
        Node_Values : Float_List;
        Data        : Float;
        Data_String : Unbounded_String;
        This_Indent : Natural := 0;
        Node_Count  : Natural := 0;
        --  Print_Tree_Node is recursive
        procedure Print_Tree_Node (Curs   : Nodes_Package.Cursor;
                                   Indent : Natural := 0) is
            use Ada.Containers;
            Node   : constant Tree_Node := Element (Curs);
            Values : constant List_Of_Float_Lists := Node.Values;
        begin
            This_Indent := Indent + 1;
            if This_Indent > 10 then
                This_Indent := 1;
            end if;

            declare
                Offset : String (1 .. This_Indent + 1) := (others => ' ');
                pos    : Natural := 1;
            begin
                while pos < This_Indent - 1 loop
                    Offset (pos .. pos + 2) := "   ";
                    pos := pos + 2;
                end loop;

                if This_Indent > 1 and then pos < This_Indent + 1 then
                    Offset (Indent) := ' ';
                end if;

                Node_Count := Node_Count + 1;
                Put_Line (Offset & "Node " & Integer'Image (Node_Count));
                Print_Node (Node, Offset);

                if Values.Is_Empty then
                    Put_Line ("Values: none.");
                else
                    Data_String := To_Unbounded_String ("Values : {");
                    for index in Values.First_Index .. Values.Last_Index loop
                        Node_Values := Values.Element (index);
                        for v_index in Node_Values.First_Index ..
                          Node_Values.Last_Index loop
                            Data := Node_Values.Element (v_index);
                            Data_String := Data_String & Float'Image (Data);
                            if not (index = Node_Values.Last_Index) then
                                Data_String := Data_String & ", ";
                            end if;
                        end loop;

                        Data_String := Data_String & " }";
                        Put_Line (Offset & To_String (Data_String));
                    end loop;
                end if;
                New_Line;

                if not Is_Leaf (Curs) then
                    Print_Tree_Node (First_Child (Curs));
                    if Child_Count (Curs) > 1 then
                        Print_Tree_Node (Next_Sibling (First_Child (Curs)));
                    end if;
                end if;
            end; --  declare block
        end Print_Tree_Node;

    begin
        Put_Line (Name);
        Print_Tree_Node (First_Child (Tree_Nodes.Root));

    end Print_Tree;

    --  -------------------------------------------------------------------------

    procedure Print_Value_List (Name    : String;
                                theList : Tree.Values_List) is
        Count : Integer := 1;
    begin
        Put (Name & ": ");
        for Index in theList.First_Index .. theList.Last_Index loop
            Put ("   " & Float'Image (theList.Element (Index)));
            Count := Count + 1;
            if Count > 10 then
                New_Line;
                Count := 1;
            end if;
        end loop;
        New_Line;
    end Print_Value_List;

    --  ------------------------------------------------------------------------

    procedure Print_Value_Data_List (Name    : String;
                                     theList : ML_Types.Value_Data_List) is
        use ML_Types;
        Value : Value_Record;
        Count : Integer := 1;
    begin
        Put (Name & ": ");
        for Index in theList.First_Index .. theList.Last_Index loop
            Value := theList.Element (Index);
            case Value.Value_Kind is
            when Boolean_Type => Put (Boolean'Image (Value.Boolean_Value));
            when Float_Type => Put (Float'Image (Value.Float_Value));
            when Integer_Type => Put (Integer'Image (Value.Integer_Value));
            when UB_String_Type => Put (To_String (Value.UB_String_Value));
            end case;
            Put ("   ");
            Count := Count + 1;
            if Count > 10 then
                New_Line;
                Count := 1;
            end if;
        end loop;
        New_Line;

    end Print_Value_Data_List;

    --  ------------------------------------------------------------------------

    procedure Print_Weights (Name : String; Data : Weights.Weight_List) is
        aWeight : Float;
        Count   : Integer := 1;
    begin
        Put_Line (Name & ": ");
        for Index in Data.First_Index .. Data.Last_Index loop
            aWeight := Data.Element (Index);
            Put (Float'Image (aWeight) & "   ");
            Count := Count + 1;
            if Count > 10 then
                New_Line;
                Count := 1;
            end if;
        end loop;
        New_Line;
    end Print_Weights;

    --  ------------------------------------------------------------------------

    procedure Print_Weights_Lists (Name : String;
                                   Data : Weights.Weight_Lists_List) is
    begin
        Put_Line (Name & ": ");
        for Index in Data.First_Index .. Data.Last_Index loop
            Print_Weights ("Output" & Integer'Image (index),
                           Data.Element (Index));
        end loop;
        New_Line;

    end Print_Weights_Lists;

    --  ------------------------------------------------------------------------

    function Set_Diff (Values, Uniques : Natural_List) return Natural_List is
        use Natural_Package;
        Unique_Vals : constant Natural_List := Encode_Utils.Unique (Values);
        aVal        : Natural;
        Diff        : Natural_List;
    begin
        for index in Unique_Vals.First_Index .. Unique_Vals.Last_Index loop
            aVal := Unique_Vals.Element (index);
            if not Uniques.Contains (aVal) then
                Diff.Append (aVal);
            end if;
        end loop;

        return Diff;
    end Set_Diff;

    --  -------------------------------------------------------------------------

    function Unique (Nums : Integer_List) return Integer_List is
        use Int_Sets;
        use Integer_Package;
        Unique_Set : Int_Sets.Set;
        Int_Curs   : Integer_Package.Cursor := Nums.First;
        Set_Curs   : Int_Sets.Cursor;
        Nums_List  : Integer_List;
    begin
        while Has_Element (Int_Curs) loop
            Unique_Set.Include (Element (Int_Curs));
            Next (Int_Curs);
        end loop;

        Set_Curs := Unique_Set.First;
        while Has_Element (Set_Curs) loop
            Nums_List.Append (Element (Set_Curs));
            Next (Set_Curs);
        end loop;
        return Nums_List;
    end Unique;

    --  -------------------------------------------------------------------------

    function Unique_Integer_Array (Nums : ML_Types.Value_Data_Array)
                                   return Integer_Array is
        use Int_Sets;
        Unique_Set : Int_Sets.Set;
        Set_Curs   : Int_Sets.Cursor;
    begin
        for index in Nums'Range loop
            Unique_Set.Include (Nums (index).Integer_Value);
        end loop;

        declare
            Unique_Array : Integer_Array (1 .. Integer (Unique_Set.Length));
            Unique_Index : Integer := 0;
        begin
            Set_Curs := Unique_Set.First;
            while Has_Element (Set_Curs) loop
                Unique_Index := Unique_Index + 1;
                Unique_Array (Unique_Index) := Element (Set_Curs);
                Next (Set_Curs);
            end loop;
            return Unique_Array;
        end;
    end Unique_Integer_Array;

    --  -------------------------------------------------------------------------

    function Unique_Integer_Array (Nums : Integer_Array) return Integer_Array is
        use Int_Sets;
        Unique_Set : Int_Sets.Set;
        Set_Curs   : Int_Sets.Cursor;
    begin
        for index in Nums'Range loop
            Unique_Set.Include (Nums (index));
        end loop;

        declare
            Unique_Array : Integer_Array (1 .. Integer (Unique_Set.Length));
            Unique_Index : Integer := 0;
        begin
            Set_Curs := Unique_Set.First;
            while Has_Element (Set_Curs) loop
                Unique_Index := Unique_Index + 1;
                Unique_Array (Unique_Index) := Element (Set_Curs);
                Next (Set_Curs);
            end loop;
            return Unique_Array;
        end;
    end Unique_Integer_Array;

    --  -------------------------------------------------------------------------

end Classifier_Utilities;
