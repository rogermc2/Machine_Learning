
with Ada.Containers;
with Ada.Text_IO; use Ada.Text_IO;

with Base_Decision_Tree;

package body Print_Utilities is

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
            raise Print_Error with
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
                                         Data : Float_List_2D) is
    begin
        Put_Line (Name & ": ");
        for index in Data.First_Index .. Data.Last_Index loop
            Print_Float_List ("List" & Integer'Image (index),
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
      (Name : String; Multi_List : Tree.Values_List_2D) is
    begin
        Print_List_Of_Float_Lists (Name, Multi_List);
    end Print_List_Of_Value_Lists;

    --  ------------------------------------------------------------------------

    procedure Print_List_Of_Value_Data_Lists
      (Name : String; Multi_List : ML_Types.Value_Data_Lists_2D) is
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
        if Node.Leaf_Node then
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

        if not Node.Leaf_Node then
            Put_Line (To_String (UB_Offset & "Feature index: " &
                        Integer'Image (Node.Feature_Index)));
            Put_Line (To_String (UB_Offset & "Threshold: " &
                        Float'Image (Node.Threshold)));
        end if;

        if Node.Values.Is_Empty then
            Put_Line (To_String (UB_Offset) &
                        "Prediction: Values list is empty!");
        else
            Print_List_Of_Value_Data_Lists
              (To_String (UB_Offset & "Prediction"), Node.Values);
        end if;

    end Print_Node;

    --  ------------------------------------------------------------------------

    procedure Print_Node (Message : String; Node : Tree.Tree_Node) is
    begin
        Put_Line (Message);
        Print_Node (node);
        New_Line;
    end Print_Node;

    --  ------------------------------------------------------------------------

    procedure Print_Node_Cursor_Array (Name    : String;
                                       Cursors : Tree.Leaf_Cursor_Array) is
        use Tree.Nodes_Package;
    begin
        Put_Line (Name & ": ");
        for Index in Cursors'First .. Cursors'Last loop
            Print_Node ("", Element (Cursors (index)));
        end loop;

    end Print_Node_Cursor_Array;

    --  ------------------------------------------------------------------------

    procedure Print_Node_Cursor_List (Name    : String;
                                      Cursors : Tree.Tree_Cursor_List) is
        use Tree.Nodes_Package;
    begin
        Put_Line (Name & ": ");
        for Index in Cursors.First_Index .. Cursors.Last_Index loop
            Print_Node ("", Element (Cursors.Element (index)));
        end loop;

    end Print_Node_Cursor_List;

    --  ------------------------------------------------------------------------

    procedure Print_Split_Record (Name : String;
                                  Data : Node_Splitter.Split_Record) is
    begin
        Put_Line (Name & ": ");
        Put_Line ("Feature_Index:  " & Integer'image (Data.Feature));
        Put_Line ("Split Row:      " & Integer'image (Data.Split_Row));
        Put_Line ("Threshold:      " & Float'image (Data.Threshold));
        Put_Line ("Improvement:    " & Float'image (Data.Improvement));
        Put_Line ("Impurity Left:  " & Float'image (Data.Impurity_Left));
        Put_Line ("Impurity Right: " & Float'image (Data.Impurity_Right));
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
        use ML_Types;
        use Tree;
        use Nodes_Package;
        Nodes       : constant Tree_Nodes := aTree.Nodes;
        Node_Values : Value_Data_List;
        Data        : Value_Record;
        Data_String : Unbounded_String;
        This_Indent : Natural := 0;
        Node_Count  : Natural := 0;
        --  Print_Tree_Node is recursive
        procedure Print_Tree_Node (Curs   : Nodes_Package.Cursor;
                                   Indent : Natural := 0) is
            use Ada.Containers;
            Node   : constant Tree_Node := Element (Curs);
            Values : constant ML_Types.Value_Data_Lists_2D  := Node.Values;
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
                            case Data.Value_Kind is
                            when Boolean_Type =>
                                Data_String := Data_String &
                                  Boolean'Image (Data.Boolean_Value);
                            when Float_Type =>
                                Data_String := Data_String & Float'Image (Data.Float_Value);
                            when Integer_Type =>
                                Data_String := Data_String &
                                  Integer'Image (Data.Integer_Value);
                            when UB_String_Type =>
                                Data_String := Data_String & Data.UB_String_Value;
                            end case;
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
        Print_Tree_Node (First_Child (Nodes.Root));

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
        Put_Line ("Print_Value_Data_List");
        if Name'Length > 0 then
            Put (Name & ": ");
        end if;

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

    procedure Print_Value_Data_List_2D
      (Name : String; theList : ML_Types.Value_Data_Lists_2D) is
    begin
        if Name'Length > 0 then
            Put_Line (Name & ":");
        end if;

        if Integer (theList.Length) = 0 then
            Put_Line ("Print_Value_Data_List_2D list is empty");
        elsif Integer (theList.Element (1).Length) = 0 then
            Put_Line ("Print_Value_Data_List_2D, first data list is empty");
        else
            for index in theList.First_Index .. theList.Last_Index loop
                Print_Value_Data_List ("", theList.Element (index));
            end loop;
        end if;

    end Print_Value_Data_List_2D;

    --  ------------------------------------------------------------------------

    procedure Print_Value_Data_List_3D
      (Name : String; theList : ML_Types.Value_Data_Lists_3D) is
    begin
        if Name'Length > 0 then
            Put_Line (Name & ":");
        end if;

        if Integer (theList.Length) = 0 then
            Put_Line ("Print_Value_Data_List_3D list is empty");
        elsif Integer (theList.Element (1).Length) = 0 then
            Put_Line ("Print_Value_Data_List_3D, first 2D list is empty");
        else
            for index in theList.First_Index .. theList.Last_Index loop
                Print_Value_Data_List_2D ("", theList.Element (index));
            end loop;
        end if;

    end Print_Value_Data_List_3D;

    --  -------------------------------------------------------------

    procedure Print_Value_Lists_3D
      (Name : String; Multi_List : Tree.Values_List_3D) is
    begin
        Put_Line (Name & ":");
        for index in Multi_List.First_Index .. Multi_List.Last_Index loop
            Print_List_Of_Value_Lists ("", Multi_List.Element (index));
        end loop;
    end Print_Value_Lists_3D;

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

end Print_Utilities;
