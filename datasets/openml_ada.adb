--  Based on scikit-learn/sklearn/datasets _openml.py

with Ada.Assertions; use Ada.Assertions;
with Ada.Containers.Ordered_Maps;
with Ada.Streams.Stream_IO;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;
--  with Ada.Text_IO.Unbounded_IO;

--  with Printing;

with Load_ARFF_Data.ARFF_IO;
--  with Load_ARFF_Data.ARFF_Printing;

--  pragma Warnings (Off);

package body Openml_Ada is

   --     type ARFF_Return_Type is (Arff_Dense, Arff_Coo, Arff_Lod,
   --                               Arff_Dense_Gen, Arff_Lod_Gen);

   package Attribute_Dictionary_Package is new
     Ada.Containers.Ordered_Maps (Unbounded_String, Positive);
   subtype Attribute_Dictionary_Map is Attribute_Dictionary_Package.Map;

   --     function Get_Num_Samples (Qualities : Qualities_Map) return Integer;
   function Parse_Nominal_Data
     (Arff_Data       : Load_ARFF_Data.ARFF_Record;
      Include_Columns : ML_Types.String_List)
      return Load_ARFF_Data.Nominal_Data_List;
   procedure Process_Feature (Features_List : Load_ARFF_Data.Attribute_List);
   procedure Save_OML
     (Save_File_Name : String; X, Y : Load_ARFF_Data.ARFF_Data_List_2D;
      Bunch          : Bunch_Data; X_Y_Only : Boolean);
   procedure Set_Default_Target
     (Features_List  : in out Load_ARFF_Data.Attribute_List;
      Target_Columns : out ML_Types.String_List);
   function Split_Columns
     (Arff_Data       : Load_ARFF_Data.ARFF_Data_List_2D;
      Include_Columns : ML_Types.Integer_DL_List)
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

   procedure Download_Data_To_Bunch
     (ARFF_Container               : Load_ARFF_Data.ARFF_Record;
      Features_List                : Load_ARFF_Data.Attribute_List;
      Data_Columns, Target_Columns : ML_Types.String_List;
      X                            : out Load_ARFF_Data.ARFF_Data_List_2D;
      Y                            : out Load_ARFF_Data.ARFF_Data_List_2D;
      Bunch                        : out Bunch_Data;
      X_Y_Only                     : Boolean := False;
      --        Sparse                     : Boolean;
      As_Frame                     : As_Frame_State := As_Frame_False) is
      --                                       Shape            : Shape_Data) is
      use Ada.Containers;
      use Load_ARFF_Data;
      use ML_Types;
      use Integer_DLL_Package;
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
      Col_Slice_X        : Integer_DL_List;
      Col_Slice_Y        : Integer_DL_List;
      --        Num_Missing        : Integer;
      --        Return_Type        : ARFF_Return_Type;
      All_Columns        : String_List;
      Nominal_Attributes : Nominal_Data_List;
      --        Frame              : Boolean := False;

      --
      --       procedure Post_Process (ARFF_Data : JSON_Value; X, Y : out JSON_Array;
      --                      Frame              : Boolean := False;
      --                      Nominal_Attributes : JSON_Array) is
      --        begin
      --           if Frame then
      --              null;
      --           end if;
      --
      --        end Post_Process;

   begin
      --        Assert (not Is_Empty (Features_List), Routine_Name &
      --                  "called with empty Features_List.");
      --        Assert (Data_Columns.Length > 0, Routine_Name &
      --                  "Data_Columns is empty.");

      --        Load_ARFF_Data.ARFF_Printing.Print_Attributes
      --          (Routine_Name & "Features_List", Features_List);

      for index in Features_List.First_Index .. Features_List.Last_Index loop
         aFeature := Features_List.Element (index);
         Features_Dict.Include (aFeature.Name, index);
      end loop;

      --        Printing.Print_Strings (Routine_Name & "Data_Columns", Data_Columns);
      --        Printing.Print_Strings (Routine_Name & "Target_Columns", Target_Columns);
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
      --        Printing.Print_Integer_List (Routine_Name & "Col_Slice_Y", Col_Slice_Y);

      --  L566 continued
      for Col_ID in Features_List.First_Index ..
        Features_List.Last_Index - Extended_Index (Col_Slice_Y.Length) loop
         aFeature := Features_List.Element (Col_ID);
         Feature_Index := Features_Dict.Element (aFeature.Name);
         Col_Slice_X.Append (Feature_Index);
      end loop;
      --        Printing.Print_Integer_List (Routine_Name & "Col_Slice_X", Col_Slice_X);

      --  L568
      while Has_Element (Columns_Curs) loop
         Col_Name := Element (Columns_Curs);
         Col_Slice_X.Append (Features_Dict.Element (Col_Name));
         Next (Columns_Curs);
      end loop;

      --  L569
      --        for index in  Col_Slice_Y.First_Index .. Col_Slice_Y.Last_Index loop
      --           Feature_Index := Col_Slice_Y.Element (index);
      --           Num_Missing := Integer'Value
      --             (Get (aFeature, "number_of_missing_values"));
      --           Assert (Num_Missing >= 0,
      --                   Routine_Name & "Target column " & " has " & " missing values."
      --                   & "Missing values are not supported for target columns.");
      --        end loop;

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
      --  L325
      Put_Line (Routine_Name & "Convert_Arff_Data");
      --  L278
      X := Split_Columns (ARFF_Container.Data, Col_Slice_X);
      Put_Line (Routine_Name & "X Split");
      Y := Split_Columns (ARFF_Container.Data, Col_Slice_Y);
      Put_Line (Routine_Name & "Y Split");

      Put_Line (Routine_Name & "X length" & Count_Type'Image (X.Length));
      Put_Line (Routine_Name & "Y length" & Count_Type'Image (Y.Length));

      --        Load_ARFF_Data.ARFF_Printing.Print_Data (Routine_Name & "X", X, 1, 2);
      --        Load_ARFF_Data.ARFF_Printing.Print_Data (Routine_Name & "Y", Y, 1, 2);
      --  L672
      if not X_Y_Only then
         Put_Line (Routine_Name & "Parse_Nominal_Data");
         Nominal_Attributes :=
           Parse_Nominal_Data (ARFF_Container, Target_Columns);
         Bunch.As_Frame := As_Frame_False;
         Bunch.Categories := Nominal_Attributes;
         Bunch.Feature_Names := Data_Columns;
         Bunch.Target_Names := Target_Columns;
      end if;

   end Download_Data_To_Bunch;

   --  ------------------------------------------------------------------------
   --  Target_Column : str, list or None, specifies the column name in the data
   --  to use as target. If empty, all columns are returned as data and the
   --  target is `None`. If a list of strings, all columns with these names
   --  are returned as a multi-target.
   procedure Fetch_Openml (Dataset_File_Name : String;
                           Save_File_Name    : String;
                           Target_Column     : ML_Types.String_List;
                           X                 : out Load_ARFF_Data.ARFF_Data_List_2D;
                           Y                 : out Load_ARFF_Data.ARFF_Data_List_2D;
                           Bunch             : out Bunch_Data;
                           As_Frame          : in out As_Frame_State;
                           Return_X_Y        : Boolean := False) is
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
      --        Put_Line (Routine_Name & "Data_Columns length: " &
      --                  Integer'Image (Integer (Data_Columns.Length)));
      --        Printing.Print_Strings (Routine_Name & "Data_Columns", Data_Columns);
      --        Printing.Print_Strings (Routine_Name & "Target_Columns", Target_Columns);
      --  L948
      --        if not Return_Sparse then
      --           Data_Qualities := Get_Data_Qualities (Data_Id);
      --           if Get_Num_Samples (Data_Qualities) > - 1 then
      --              null;
      --              --              Shape := (Get_Num_Samples (Data_Qualities), Length (Features_List));
      --           end if;
      --        end if;

      --  L955
      Download_Data_To_Bunch (ARFF_Data, Features_List, Data_Columns,
                              Target_Columns, X, Y, Bunch, Return_X_Y);
      if Save_File_Name'Length > 0 then
         Save_OML (Save_File_Name, X, Y, Bunch, Return_X_Y);
      end if;

   end Fetch_Openml;

   --  ------------------------------------------------------------------------
   --     function Get_Data_Qualities (Data_ID : Integer) return Qualities_Map is
   --        use Ada.Strings;
   --        Routine_Name  : constant String := "Openml_Ada.Get_Data_Qualities ";
   --        Json_Data     : JSON_Value;
   --        Qualities     : JSON_Value;
   --        Quality_Array : Qualities_Map;
   --
   --        procedure Get_Quality (Name : Utf8_String; Value : JSON_Value) is
   --           Quality : constant JSON_Value := Create_Object;
   --        begin
   --           Quality.Set_Field (Name, Value);
   --           Append (Quality_Array, Quality);
   --
   --        end Get_Quality;
   --
   --     begin
   --        declare
   --           File_Name : constant String := "../dataset_" &
   --                         Fixed.Trim (Integer'Image (Data_ID), Both) &
   --                         "_qualities";
   --        begin
   --           Json_Data := Get_Json_Content_From_File (File_Name);
   --        end;
   --
   --        if Has_Field (Json_Data, "data_qualities") then
   --           Qualities := Get (Json_Data, "data_qualities");
   --           Map_JSON_Object (Qualities, Get_Quality'access);
   --        else
   --           Put_Line
   --             (Routine_Name & "Qualities file with" &
   --                Integer'Image (Data_ID) &
   --                " does not have a data_qualities field.");
   --        end if;
   --
   --        return Quality_Array;
   --
   --     end Get_Data_Qualities;

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
                  Nominal : constant Nominal_Data_Record :=
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
   procedure Set_Default_Target (Features_List  : in out Load_ARFF_Data.Attribute_List;
                                 Target_Columns : out ML_Types.String_List) is
      use Load_ARFF_Data;
      use Attribute_Data_Package;
      --        Routine_Name  : constant String := "Openml_Ada.Set_Default_Target ";
      Feature          : Attribute_Record;
   begin
      Feature := Features_List.Element (Features_List.Last_Index);
      Feature.Is_Target := True;
      Features_List.Replace_Element (Features_List.Last_Index, Feature);
      Target_Columns.Append (Feature.Name);

   end Set_Default_Target;

   --  ------------------------------------------------------------------------

   procedure Save_OML
     (Save_File_Name : String; X, Y : Load_ARFF_Data.ARFF_Data_List_2D;
      Bunch          : Bunch_Data; X_Y_Only : Boolean) is
      use Ada.Streams;
      use Stream_IO;
      use Load_ARFF_Data;
      File_ID  : Stream_IO.File_Type;
      aStream  : Stream_Access;
   begin
      Create (File_ID, Out_File, Save_File_Name);
      aStream := Stream (File_ID);
      ARFF_Data_List_2D'Write (aStream, X);
      ARFF_Data_List_2D'Write (aStream, Y);
      if not X_Y_Only then
         Bunch_Data'Write (aStream, Bunch);
      end if;
      Close (File_ID);
      pragma Unreferenced (File_ID);

   end Save_OML;

   --  ------------------------------------------------------------------------
   --  L184
   function Split_Columns
     (Arff_Data       : Load_ARFF_Data.ARFF_Data_List_2D;
      Include_Columns : ML_Types.Integer_DL_List)
      return Load_ARFF_Data.ARFF_Data_List_2D is
      use ML_Types;
      use Integer_DLL_Package;
      use Load_ARFF_Data;
      use ARFF_Data_List_Package;
      use ARFF_Data_Package;
      --        Routine_Name  : constant String := "Openml_Ada.Split_Columns ";
      Arff_Data_New : ARFF_Data_List_2D;
      Include_Curs  : Integer_DLL_Package.Cursor;
      New_Row       : ARFF_Data_List;
      Arff_Data_Row : ARFF_Data_List;  --  list of columns
   begin
      for row in Arff_Data.First_Index .. Arff_Data.Last_Index loop
         New_Row.Clear;
         Arff_Data_Row := Arff_Data.Element (row);
         Include_Curs := Include_Columns.First;
         while Has_Element (Include_Curs) loop
            New_Row.Append (Arff_Data_Row.Element (Element (Include_Curs)));
            Next  (Include_Curs);
         end loop;
         --           Load_ARFF_Data.ARFF_Printing.Print_Data
         --             (Routine_Name & "New_Row", New_Row);
         Arff_Data_New.Append (New_Row);
      end loop;

      return Arff_Data_New;

   end Split_Columns;

   --  ------------------------------------------------------------------------
   --  L699
   function Valid_Data_Column_Names
     (Features_List  : Load_ARFF_Data.Attribute_List;
      Target_Columns : ML_Types.String_List) return ML_Types.String_List is
      --        use Ada.Text_IO.Unbounded_IO;
      use Load_ARFF_Data;
      --        Routine_Name  : constant String := "Openml_Ada.Valid_Data_Column_Names ";
      Feature       : Attribute_Record;
      Feature_Name  : Unbounded_String;
      Valid_Names   : ML_Types.String_List;

      function Is_A_Target return Boolean is
         use ML_Types;
         use String_Package;
         Target_Curs  : String_Package.Cursor := Target_Columns.First;
         Target_Found : Boolean := False;
      begin
         --  L707
         while Has_Element (Target_Curs) and not Target_Found loop
            --              Put_Line (Routine_Name & ".Is_A_Target target, feature " &
            --                       Element (Target_Curs) & ", " & Feature_Name);
            Target_Found := Element (Target_Curs) = Feature_Name;
            Next (Target_Curs);
         end loop;

         return Target_Found;

      end Is_A_Target;

   begin
      --  L705
      for index in Features_List.First_Index .. Features_List.Last_Index loop
         Feature := Features_List.Element (index);
         Feature_Name := Feature.Name;
         if not Is_A_Target and then
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
