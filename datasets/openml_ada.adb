--  Based on scikit-learn/sklearn/datasets _openml.py

with Ada.Assertions; use Ada.Assertions;
with Ada.Containers.Ordered_Maps;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;
--  with Ada.Text_IO.Unbounded_IO;

with Printing;

with Load_ARFF_Data.ARFF_IO;
with Load_ARFF_Data.ARFF_Printing;

--  pragma Warnings (Off);

package body Openml_Ada is

   --     type ARFF_Return_Type is (Arff_Dense, Arff_Coo, Arff_Lod,
   --                               Arff_Dense_Gen, Arff_Lod_Gen);

   package Attribute_Dictionary_Package is new
     Ada.Containers.Ordered_Maps (Unbounded_String, Positive);
   subtype Attribute_Dictionary_Map is Attribute_Dictionary_Package.Map;
   --
   --     use Tupple_Package;
   --     package Zip_Package is new
   --       Ada.Containers.Vectors (Positive, Tupple_Vector);
   --     subtype Zip_Vector is Zip_Package.Vector;

   --     package Pair_Settings_Vector_Package is new
   --       Ada.Containers.Doubly_Linked_Lists (JSON_Item);
   --     subtype Pair_List is Pair_Settings_Vector_Package.List;

   --      package ML_Names_Package is new
   --        Ada.Containers.Doubly_Linked_Lists (Unbounded_String);
   --      subtype Names_List is ML_Names_Package.List;
   --
   --      package ML_Features_Package is new
   --        Ada.Containers.Ordered_Maps (Unbounded_String, Unbounded_String);
   --      subtype Features_Map is ML_Features_Package.Map;

   function Get_Json_Content_From_File (File_Name : String) return JSON_Value;
   --     function Get_Num_Samples (Qualities : Qualities_Map) return Integer;
   function Parse_Nominal_Data
     (Arff_Data       : Load_ARFF_Data.ARFF_Record;
      Include_Columns : ML_Types.String_List)
       return Load_ARFF_Data.Nominal_Data_List;
   procedure Process_Feature (Features_List : Load_ARFF_Data.Attribute_List);
   procedure Set_Default_Target (Features_List  : Load_ARFF_Data.Attribute_List;
                                 Target_Columns : out ML_Types.String_List);
   function Split_Sparse_Columns
     (Arff_Data       : Load_ARFF_Data.ARFF_Data_List_2D;
      Include_Columns : ML_Types.Integer_List)
       return Load_ARFF_Data.ARFF_Data_List_2D;
   procedure Verify_Target_Data_Type
     (Features_Dict  : Attribute_Dictionary_Map;
      Target_Columns : ML_Types.String_List);

   --  ------------------------------------------------------------------------

   --     function Convert_Arff_Data_Dataframe
   --       (ARFF_Container : ARFF.Arff_Container_Type; Features : JSON_Value)
   --        return JSON_Value is
   --        Routine_Name    : constant String := "Opemml.Convert_Arff_Data_Dataframe";
   --        Description     : constant JSON_Array :=
   --                            Arff_Container.Get ("description");
   --        Relation        : constant String :=
   --                            Arff_Container.Get ("relation");
   --        Attributes      : constant JSON_Array :=
   --                            Arff_Container.Get ("attributes");
   --        ARFF_Data       : constant JSON_Array :=
   --                            Arff_Container.Get ("data");
   --        First_Row       : constant JSON_Value :=
   --                            Array_Element (ARFF_Data, Array_First (ARFF_Data));
   --        Result          : JSON_Value;
   --     begin
   --        return Result;
   --
   --     end Convert_Arff_Data_Dataframe;

   --  ------------------------------------------------------------------------
   --  ArffSparseDataType = Tuple[List, ...] A Tuple is a collection of Python
   --  objects separated by commas. L325
   procedure Convert_Arff_Data
     (Arff_Container            : Load_ARFF_Data.ARFF_Record;
      Col_Slice_X, Col_Slice_Y  : ML_Types.Integer_List;
      X, Y                      : out  Load_ARFF_Data.ARFF_Data_List_2D) is
      use Load_ARFF_Data;
      --          Routine_Name    : constant String := "Openml_Ada.Convert_Arff_Data ";
      ARFF_Data  : constant ARFF_Data_List_2D := Get_Data (Arff_Container);
   begin
      --  L278
      X := Split_Sparse_Columns (ARFF_Data, Col_Slice_X);
      Y := Split_Sparse_Columns (ARFF_Data, Col_Slice_Y);

   end Convert_Arff_Data;

   --  ------------------------------------------------------------------------

   function Download_Data_To_Bunch
     (ARFF_Container               : Load_ARFF_Data.ARFF_Record;
      Features_List                : Load_ARFF_Data.Attribute_List;
      Data_Columns, Target_Columns : ML_Types.String_List;
      Return_X_Y                   : Boolean := False;
      --        Sparse         : Boolean;
      As_Frame                     : As_Frame_State := As_Frame_False)
     --                                       Shape            : Shape_Data)
       return Bunch_Data is
      use Ada.Containers;
      use Load_ARFF_Data;
      use ML_Types;
      use String_Package;
      use Attribute_Data_Package;
      Routine_Name       : constant String :=
                               "Openml_Ada.Download_Data_To_Bunch ";
      Columns_Curs       : String_Package.Cursor := Data_Columns.First;
      Target_Curs        : String_Package.Cursor := Target_Columns.First;
      Feature_Index      : Positive;
      Features_Dict      : Attribute_Dictionary_Map;
      aFeature           : Attribute_Record;
      Col_Name           : Unbounded_String;
      Col_Slice_X        : Integer_List;
      Col_Slice_Y        : Integer_List;
      --        Num_Missing        : Integer;
      --        Return_Type        : ARFF_Return_Type;
      All_Columns        : String_List;
      X                  : ARFF_Data_List_2D;
      Y                  : ARFF_Data_List_2D;
      Nominal_Attributes : Nominal_Data_List;
      --        Frame              : Boolean := False;
      Bunch              : Bunch_Data (Return_X_Y);

      procedure Parse_ARFF
        (ARFF_In          : ARFF_Record;
         X_Slice, Y_Slice : ML_Types.Integer_List;
         X_out, Y_out     : out ARFF_Data_List_2D;
         Nominal_Data_Out : out Nominal_Data_List) is
      begin
         Convert_Arff_Data (ARFF_In, X_Slice, Y_Slice, X_out, Y_out);
         Nominal_Data_Out := Parse_Nominal_Data (ARFF_In, Target_Columns);
--           Put_Line (Routine_Name & "Parse_ARFF Nominal_Data_Out length: " &
--                       Count_Type'Image (Nominal_Data_Out.Length));

      end Parse_ARFF;

      --        procedure
      --        Post_Process (ARFF_Data : JSON_Value; X, Y : out JSON_Array;
      --                      Frame              : Boolean := False;
      --                      Nominal_Attributes : JSON_Array) is
      --        begin
      --           if Frame then
      --              null;
      --           end if;
      --
      --        end Post_Process;

   begin
      Assert (not Is_Empty (Features_List), Routine_Name &
                "called with empty Features_List.");
      Assert (Data_Columns.Length > 0, Routine_Name &
                "Data_Columns is empty.");
      Assert (Target_Columns.Length = Data_Columns.Length, Routine_Name &
                " Target_Columns length" &
                Count_Type'Image (Target_Columns.Length) &
                " is different to Data_Columns length" &
                Count_Type'Image (Data_Columns.Length));

      Load_ARFF_Data.ARFF_Printing.Print_Attributes
          (Routine_Name &"Features_List", Features_List);

      for index in Features_List.First_Index .. Features_List.Last_Index loop
         aFeature := Features_List.Element (index);
         Features_Dict.Include (aFeature.Name, index);
      end loop;

      Verify_Target_Data_Type (Features_Dict, Target_Columns);

      --  L566 col_slice_y =
      --        [
      --          int(features_dict[col_name]["index"])
      --          for col_name in target_columns
      --        ]
      --  target_columns is a list of feature names
      while Has_Element (Target_Curs) loop
         Col_Name := Element (Target_Curs);
         Feature_Index := Features_Dict.Element (Col_Name);
         --        Put_Line (Routine_Name & "aFeature Y " & aFeature.Write);
         Col_Slice_Y.Append (Feature_Index);
         Next (Target_Curs);
      end loop;
      Printing.Print_Integer_List (Routine_Name & "Col_Slice_Y", Col_Slice_Y);

      --  L566 continued
      for Col_ID in Features_List.First_Index .. Features_List.Last_Index loop
         aFeature := Features_List.Element (Col_ID);
         Feature_Index := Features_Dict.Element (aFeature.Name);
         Col_Slice_X.Append (Feature_Index);
      end loop;
      Printing.Print_Integer_List (Routine_Name & "Col_Slice_X", Col_Slice_X);

      --  L568
      while Has_Element (Columns_Curs) loop
         Col_Name := Element (Columns_Curs);
         Col_Slice_X.Append (Features_Dict.Element (Col_Name));
         Next (Columns_Curs);
      end loop;

      --  L569
      for index in  Col_Slice_Y.First_Index .. Col_Slice_Y.Last_Index loop
         Feature_Index := Col_Slice_Y.Element (index);
         --           Num_Missing := Integer'Value
         --             (Get (aFeature, "number_of_missing_values"));
         --           Assert (Num_Missing >= 0,
         --                   Routine_Name & "Target column " & " has " & " missing values."
         --                   & "Missing values are not supported for target columns.");
      end loop;

      --  L582
      --        if Sparse then
      --           Return_Type := Arff_Coo;
      --        else
      --           Return_Type := Arff_Dense;
      --        end if;

      --  L601
      if As_Frame = As_Frame_True then
         All_Columns := Data_Columns;
         Target_Curs := Target_Columns.First;
         while Has_Element (Target_Curs) loop
            All_Columns.Append (Element (Target_Curs));
            Next (Target_Curs);
         end loop;
      end if;

      --  L667
      Parse_ARFF (ARFF_Container, Col_Slice_X, Col_Slice_Y, X, Y,
                  Nominal_Attributes);
--        Put_Line (Routine_Name & "X length" & Count_Type'Image (X.Length));
--        Put_Line (Routine_Name & "Y length" & Count_Type'Image (Y.Length));

      Load_ARFF_Data.ARFF_Printing.Print_Data (Routine_Name & "X", X, 1, 2);
      --  L672
      Bunch.Data := X;
      Bunch.Target := Y;
      if not Return_X_Y then
         Bunch.As_Frame := As_Frame_False;
         Bunch.Categories := Nominal_Attributes;
         Bunch.Feature_Names := Data_Columns;
         Bunch.Target_Names := Target_Columns;
      end if;

      return Bunch;

   end Download_Data_To_Bunch;

   --  ------------------------------------------------------------------------
   --  Target_Column : str, list or None, specifies the column name in the data
   --  to use as target. If empty, all columns are returned as data and the
   --  target is `None`. If a list of strings, all columns with these names
   --  are returned as a multi-target.
   function Fetch_Openml (Dataset_File_Name : String;
                          Target_Column     : ML_Types.String_List;
                          As_Frame          : in out As_Frame_State;
                          Return_X_Y        : Boolean := False)
                           return Bunch_Data is
      use Ada.Directories;
      use Ada.Strings;
      use Load_ARFF_Data;
      use ARFF_IO;
      use ML_Types.String_Package;
      Routine_Name    : constant String := "Openml_Ada.Fetch_Openml ";
      Pos             : constant Positive
        := Fixed.Index (Dataset_File_Name, ".",
                        Dataset_File_Name'Last, Backward);
      Ada_File        : constant String
        := Slice (To_Unbounded_String (Dataset_File_Name),
                  Dataset_File_Name'First, Pos) & "ada";
      ARFF_Data       : ARFF_Record;
      Return_Sparse   : constant Boolean := False;
      Features_List   : Attribute_List;
      Curs            : Cursor;
      Target_Value    : Unbounded_String;
      Target_Columns  : ML_Types.String_List;
      Data_Columns    : ML_Types.String_List;
      --        Shape           : Shape_Data;
      --        Data_Qualities  : Qualities_Map;
      Bunch           : Bunch_Data (Return_X_Y);
   begin
      if Exists (Ada_File) then
         Put_Line (Routine_Name & "Reading data file " & Ada_File);
         Read_ARFF_Ada (Ada_File, ARFF_Data);
         Put_Line (Routine_Name & "Data file read");
      else
         Put_Line (Routine_Name & "Loading ARFF data from " & Dataset_File_Name);
         Load_ARFF (Dataset_File_Name, ARFF_Data);
         Put_Line (Routine_Name & "Data loaded");
         Save_ARFF (Ada_File, ARFF_Data);
      end if;
      New_Line;
      --  L903
      if As_Frame = As_Frame_Auto then
         if not Return_Sparse then
            As_Frame := As_Frame_True;
         end if;
      else
         As_Frame := As_Frame_False;
      end if;

      Assert (not (As_Frame = As_Frame_True and Return_Sparse),
              Routine_Name & "cannot return dataframe with sparse data");

      --  L917
      Features_List := Get_Attributes (ARFF_Data);

      if As_Frame = As_Frame_False then
         Process_Feature (Features_List);
      end if;

      --  L929
      if Target_Column.Is_Empty then
         Set_Default_Target (Features_List, Target_Columns);
      else
         Curs := Target_Column.First;
         while Has_Element (Curs) loop
            Target_Value := Element (Curs);
            Target_Value := To_Unbounded_String
              (Slice (Target_Value, 2, Length (Target_Value)));
            Trim (Target_Value, Both);
            Target_Columns.Append (Target_Value);
            Next (Curs);
         end loop;
      end if;

      --  L944
      Data_Columns := Valid_Data_Column_Names (Features_List, Target_Columns);
      --          Put_Line (Routine_Name & "Features_List length: " &
      --                        Integer'Image (Integer (Features_List.Length)));
      --          Put_Line (Routine_Name & "Target_Columns length: " &
      --                        Integer'Image (Integer (Target_Columns.Length)));
      --          Put_Line (Routine_Name & "Data_Columns length: " &
      --                        Integer'Image (Integer (Data_Columns.Length)));
      --  L948
      --        if not Return_Sparse then
      --           Data_Qualities := Get_Data_Qualities (Data_Id);
      --           if Get_Num_Samples (Data_Qualities) > - 1 then
      --              null;
      --              --              Shape := (Get_Num_Samples (Data_Qualities), Length (Features_List));
      --           end if;
      --        end if;

      --  L955
      Bunch := Download_Data_To_Bunch
        (ARFF_Data, Features_List, Data_Columns, Target_Columns, Return_X_Y);

      return Bunch;

   end Fetch_Openml;

   --  ------------------------------------------------------------------------

   function Get_Data_Description_By_ID
     (Data_ID : Integer) return JSON_Value is
      use Ada.Strings;
      Routine_Name : constant String := "Openml_Ada.Get_Data_Description_By_ID ";
      Data_Desc    : JSON_Value := Create_Object;
   begin
      declare
         File_Name : constant String := "../dataset_" &
                       Fixed.Trim (Integer'Image (Data_ID), Both) &
                       "_description";
      begin
         Data_Desc := Get_Json_Content_From_File (File_Name);
      end;

      --           Put_Line (Routine_Name & "Data_Desc empty? " &
      --                      Boolean'Image (Is_Empty (Data_Desc)));
      if Has_Field (Data_Desc, "data_set_description") then
         --              Put_Line (Routine_Name &
         --                          "Data_Desc has data_set_description field");
         Data_Desc := Get (Data_Desc, "data_set_description");
      else
         Put_Line (Routine_Name & "Data_Desc is not a data_set_description");
      end if;

      return Data_Desc;

   end Get_Data_Description_By_ID;

   --  ------------------------------------------------------------------------

   function Get_Data_Features (Data_ID   : Integer) return JSON_Array is
      use Ada.Strings;
      Routine_Name  : constant String := "Openml_Ada.Get_Data_Features ";
      Json_Data     : JSON_Value := Create_Object;
      Features      : JSON_Value := Create_Object;
      Feature       : JSON_Value := Create_Object;
      Feature_Array : JSON_Array;
   begin
      declare
         File_Name : constant String := "../dataset_" &
                       Fixed.Trim (Integer'Image (Data_ID), Both) &
                       "_features";
      begin
         --              Put_Line (Routine_Name & "File_Name: " & File_Name);
         Json_Data := Get_Json_Content_From_File (File_Name);
      end;

      Assert (Has_Field (Json_Data, "data_features") or
                Has_Field (Json_Data, "features"), Routine_Name &
                "data_features is not a Json_Data field.");

      if Has_Field (Json_Data, "data_features") then
         Features := Get (Json_Data, "data_features");
         Assert (Has_Field (Features, "feature"), Routine_Name &
                   "data_features is not a Json_Data field.");
         Feature := Get (Features, "feature");
         Feature_Array := Get (Feature);
      else
         Feature_Array := Get (Json_Data, "features");
      end if;

      return Feature_Array;

   end Get_Data_Features;

   --  ------------------------------------------------------------------------
   --  L384
   function Get_Data_Info_By_Name (Dataset_Name : String)
                                    return JSON_Value is
      --        Routine_Name   : constant String := "Openml_Ada.Get_Data_Info_By_Name ";
      Json_Data      : JSON_Value;
   begin
      declare
         File_Name : constant String := "../" & Dataset_Name & "_info";
      begin
         Json_Data := Get_Json_Content_From_File (File_Name);
      end;

      return Json_Data;

   end Get_Data_Info_By_Name;

   --  ------------------------------------------------------------------------

   function Get_Data_Qualities (Data_ID : Integer) return Qualities_Map is
      use Ada.Strings;
      Routine_Name  : constant String := "Openml_Ada.Get_Data_Qualities ";
      Json_Data     : JSON_Value;
      Qualities     : JSON_Value;
      Quality_Array : Qualities_Map;

      procedure Get_Quality (Name : Utf8_String; Value : JSON_Value) is
         Quality : constant JSON_Value := Create_Object;
      begin
         Quality.Set_Field (Name, Value);
         Append (Quality_Array, Quality);

      end Get_Quality;

   begin
      declare
         File_Name : constant String := "../dataset_" &
                       Fixed.Trim (Integer'Image (Data_ID), Both) &
                       "_qualities";
      begin
         Json_Data := Get_Json_Content_From_File (File_Name);
      end;

      if Has_Field (Json_Data, "data_qualities") then
         Qualities := Get (Json_Data, "data_qualities");
         Map_JSON_Object (Qualities, Get_Quality'access);
      else
         Put_Line
           (Routine_Name & "Qualities file with" &
              Integer'Image (Data_ID) &
              " does not have a data_qualities field.");
      end if;

      return Quality_Array;

   end Get_Data_Qualities;

   --  ------------------------------------------------------------------------

   function Get_Json_Content_From_File (File_Name : String) return JSON_Value is
      --       Routine_Name   : constant String :=
      --                           "Openml_Ada.Get_Json_Content_From_File ";
      Name           : constant String := File_Name & ".json";
      File           : File_Type;
      JSON_Data      : Unbounded_String;
      JSON_Main_Node : JSON_Value := Create_Object;
   begin
      Open (File, In_File, Name);
      while not End_Of_File (File) loop
         Append (JSON_Data, To_Unbounded_String (Get_Line (File)));
      end loop;
      Close (File);

      JSON_Main_Node := GNATCOLL.JSON.Read (Strm => JSON_Data, Filename => "");

      return JSON_Main_Node;

   end Get_Json_Content_From_File;

   --  ------------------------------------------------------------------------

   --     function Get_Num_Samples (Qualities : Qualities_Map) return Integer is
   --        --        Routine_Name  : constant String := "Openml_Ada.Get_Num_Samples ";
   --        Quality       : JSON_Value := Create;
   --        Index         : Positive := Array_First (Qualities);
   --        Num_Samples   : Integer := -1;
   --
   --        procedure Get_Num_Instances (Name : Utf8_String; Value : JSON_Value) is
   --           Num_Instances : Float := 0.0;
   --        begin
   --           if Name = "NumberOfInstances" then
   --              Num_Instances := Get (Value);
   --              Num_Samples := Integer (Num_Instances);
   --           end if;
   --        end Get_Num_Instances;
   --
   --        procedure Get_Qual (Name : Utf8_String; Value : JSON_Value) is
   --           Name_Quality : constant JSON_Value := Create_Object;
   --        begin
   --           if Name = "name" and then Kind (Value) = JSON_String_Type then
   --              declare
   --                 String_Value : constant String := Get (Value);
   --              begin
   --                 if String_Value = "value" then
   --                    Name_Quality.Set_Field (Name, Value);
   --                 end if;
   --                 Map_JSON_Object (Name_Quality, Get_Num_Instances'access);
   --              end;
   --           end if;
   --
   --        end Get_Qual;
   --
   --     begin
   --        while Array_Has_Element (Qualities, Index) loop
   --           Quality := Array_Element (Qualities, Index);
   --           Map_JSON_Object (Quality, Get_Qual'access);
   --           Index := Array_Next (Qualities, Index);
   --        end loop;
   --
   --        return Num_Samples;
   --
   --     end Get_Num_Samples;

   --  ------------------------------------------------------------------------

   function Parse_Nominal_Data
     (Arff_Data       : Load_ARFF_Data.ARFF_Record;
      Include_Columns : ML_Types.String_List)
      return Load_ARFF_Data.Nominal_Data_List is
--        use Ada.Containers;
      use ML_Types;
      use String_Package;
      use Load_ARFF_Data;
      use Nominal_Data_Package;
--        Routine_Name  : constant String := "Openml_Ada.Parse_Nominal_Data ";
      Attributes    : constant Attribute_List := Get_Attributes (Arff_Data);
      Include_Curs  : String_Package.Cursor := Include_Columns.First;
      Attribute     : Attribute_Record;
      Nominal_Data  : Nominal_Data_List;
      Nominal_Curs  : Nominal_Data_Package.Cursor;
   begin
--         Put_Line (Routine_Name & "Include_Columns length: " &
--                       Count_Type'Image (Include_Columns.Length));
      while Has_Element (Include_Curs) loop
         for Index_V in Attributes.First_Index .. Attributes.Last_Index loop
--              Put_Line  (Routine_Name & "Index_V: " & Integer'Image (Index_V));
            Attribute := Attributes.Element (Index_V);
            Nominal_Curs := Attribute.Nominal_Data.First;
            while Has_Element (Nominal_Curs) loop
               declare
                  Nominal: constant Nominal_Data_Record :=
                             Element (Nominal_Curs);
               begin
                  Nominal_Data.Append (Nominal);
               end;
               Next  (Nominal_Curs);
            end loop;
         end loop;
         Next (Include_Curs);
      end loop;
--         Put_Line (Routine_Name & "Nominal_Data length: " &
--                       Count_Type'Image (Nominal_Data.Length));
--        New_Line;

      return Nominal_Data;

   end Parse_Nominal_Data;

   --  ------------------------------------------------------------------------

   procedure Process_Feature (Features_List : Load_ARFF_Data.Attribute_List) is
      use Load_ARFF_Data;
      Routine_Name   : constant String := "Openml_Ada.Process_Feature ";
      Attribute      : Attribute_Record;
      Ignore         : Boolean := False;
      Is_Row_ID      : Boolean := False;
      Data_Type_Item : ARFF_Data_Type;

      procedure Process_Status (Ignore_Status, Row_ID_Status : Boolean) is
      begin
         --  L921
         if not Ignore_Status and not Row_ID_Status then
            --  923
            Assert (Data_Type_Item /= ARFF_String, Routine_Name
                    & " invalid as STRING attributes are not " &
                      "supported for array representation. " &
                      "Try as_frame=True");
         end if;
      end Process_Status;

   begin
      --  L920
      for index in Features_List.First_Index .. Features_List.Last_Index loop
         Attribute := Features_List.Element (index);
         Ignore := Attribute.Ignore;
         Is_Row_ID := Attribute.Is_Row_ID;
         Data_Type_Item := Attribute.Data_Kind;
         Process_Status (Ignore, Is_Row_ID);
      end loop;

   end Process_Feature;

   --  ------------------------------------------------------------------------
   --  L922
   procedure Set_Default_Target (Features_List  : Load_ARFF_Data.Attribute_List;
                                 Target_Columns : out ML_Types.String_List) is
      use Load_ARFF_Data;
      use Attribute_Data_Package;
      --        Routine_Name  : constant String := "Openml_Ada.Set_Default_Target ";
      Feature          : Attribute_Record;
      Target_Specified : Boolean := False;
   begin
      for index in Features_List.First_Index .. Features_List.Last_Index loop
         Feature := Features_List.Element (index);
         if Feature.Is_Target then
            Target_Specified := True;
            Target_Columns.Append (Feature.Name);
         end if;
      end loop;

      if not Target_Specified then
         for index in Features_List.First_Index ..
           Features_List.Last_Index loop
            Feature := Features_List.Element (index);
            Target_Columns.Append (Feature.Name);
         end loop;
      end if;

   end Set_Default_Target;

   --  ------------------------------------------------------------------------
   --  L184 ArffSparseDataType = Tuple[List, ...] _split_sparse_columns
   --  (arff_data: ArffSparseDataType, include_columns: List) - >
   --  ArffSparseDataType Arff_Sparse_Data_Type is a subtype of JSON_Array
   function Split_Sparse_Columns
     (Arff_Data       : Load_ARFF_Data.ARFF_Data_List_2D;
      Include_Columns : ML_Types.Integer_List)
       return Load_ARFF_Data.ARFF_Data_List_2D is
      use Load_ARFF_Data;
      use ARFF_Data_List_Package;
      use ARFF_Data_Package;
--        Routine_Name    : constant String := "Openml_Ada.Split_Sparse_Columns ";
      Col_Curs        : ARFF_Data_Package.Cursor;
      Arff_Data_New   : ARFF_Data_List_2D;
      New_Row         : ARFF_Data_List;
      Arff_Data_Row   : ARFF_Data_List;
      Select_Col      : Boolean;
   begin
      for sample in Arff_Data.First_Index .. Arff_Data.Last_Index loop
         New_Row.Clear;
         Arff_Data_Row := Arff_Data.Element (sample);
         Col_Curs := Arff_Data_Row.First;
         while Has_Element (Col_Curs) loop

            for col_index in Include_Columns.First_Index ..
              Include_Columns.Last_Index loop
               Select_Col := False;
               for include_col in Include_Columns.First_Index ..
                 Include_Columns.Last_Index loop
                  Select_Col := Select_Col or
                    col_index = Include_Columns.Element (include_col);
               end loop;

               if Select_Col then
                  New_Row.Append (Element (Col_Curs));
               end if;
            end loop;

            Next (Col_Curs);
         end loop;

         Arff_Data_New.Append (New_Row);
      end loop;

      return Arff_Data_New;

   end Split_Sparse_Columns;

   --  ------------------------------------------------------------------------

   --     function J_Array_To_String_List (J_Array : JSON_Array)
   --                                      return ML_Types.String_List is
   --        use ML_Types;
   --        --        Routine_Name  : constant String := "Openml_Ada.J_Array_To_String_List ";
   --        theList       : String_List;
   --        Index         : Positive := Array_First (J_Array);
   --        J_Item        : JSON_Value;
   --        Item          : Unbounded_String;
   --     begin
   --        --  L707
   --        --           Put_Line (Routine_Name & "Feature_Val: " & To_String (Feature_Val));
   --        while Has_Element (Target_Curs) and not Target_Found loop
   --           Target := Element (Target_Curs);
   --           Target_Found := Target = Feature_Name;
   --           Next (Target_Curs);
   --        end loop;
   --
   --        return theList;
   --
   --     end J_Array_To_String_List;

   --  ------------------------------------------------------------------------
   --  L699
   function Valid_Data_Column_Names
     (Features_List  : Load_ARFF_Data.Attribute_List;
      Target_Columns : ML_Types.String_List) return ML_Types.String_List is
      --          use Ada.Text_IO.Unbounded_IO;
      use Load_ARFF_Data;
      --          Routine_Name  : constant String := "Openml_Ada.Valid_Data_Column_Names ";
      Feature       : Attribute_Record;
      Feature_Name  : Unbounded_String;
      Valid_Names   : ML_Types.String_List;

      function Check_Target return Boolean is
         use ML_Types;
         use String_Package;
         Target_Curs  : String_Package.Cursor := Target_Columns.First;
         Target_Found : Boolean := False;
      begin
         --  L707
         while Has_Element (Target_Curs) and not Target_Found loop
            Target_Found := Element (Target_Curs) = Feature_Name;
            Next (Target_Curs);
         end loop;

         return Target_Found;

      end Check_Target;

   begin
      --  L705
      for index in Features_List.First_Index .. Features_List.Last_Index loop
         Feature := Features_List.Element (index);
         Feature_Name := Feature.Name;
         if Check_Target and then
           (not Feature.Ignore and not Feature.Is_Row_ID) then
            Valid_Names.Append (Feature_Name);
         end if;
      end loop;

      return Valid_Names;

   end Valid_Data_Column_Names;

   --  ------------------------------------------------------------------------

   procedure Verify_Target_Data_Type
     (Features_Dict  : Attribute_Dictionary_Map;
      Target_Columns : ML_Types.String_List) is
      Routine_Name  : constant String := "Openml_Ada.Verify_Target_Data_Type ";
      use ML_Types.String_Package;
      Curs          : Cursor := Target_Columns.First;
      Column        : Unbounded_String;
   begin
      --        Put_Line (Routine_Name & "Target_Columns length" &
      --                    Integer'Image (Length (Target_Columns)));
      while Has_Element (Curs) loop
         Column := Element (Curs);
         Assert (Features_Dict.Contains (Column),
                 Routine_Name & "Features_Dict does not contain " &
                   To_String (Column));
         Next (Curs);
      end loop;

   end Verify_Target_Data_Type;

   --  ------------------------------------------------------------------------

end Openml_Ada;
