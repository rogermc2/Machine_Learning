--  Based on scikit-learn/sklearn/datasets _openml.py

with Ada.Assertions; use Ada.Assertions;
with Ada.Characters.Handling;
with Ada.Containers;
with Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;
--  with Ada.Text_IO.Unbounded_IO;

--  with Printing;

with ARFF_Parser;
with Load_ARFF_Data.ARFF_IO;
--  with Load_ARFF_Data.ARFF_Printing;

--  pragma Warnings (Off);

package body Openml_Ada is

   --     type ARFF_Return_Type is (Arff_Dense, Arff_Coo, Arff_Lod,
   --                               Arff_Dense_Gen, Arff_Lod_Gen);

   --     function Get_Num_Samples (Qualities : Qualities_Map) return Integer;
   procedure Get_OML (File_Name : String;
                      --                        X_Indices : out Integer_List;
                      --                        Y_Indices : out Integer_List;
                      Bunch     : out Bunch_Data);
   function Parse_Nominal_Data
     (Arff_Data       : AR_Types.ARFF_Record;
      Include_Columns : String_List)
      return AR_Types.Nominal_Data_List;
   procedure Process_Feature (Features_List : AR_Types.Attribute_List);
   procedure Save_OML
     (Save_File_Name        : String;
      --        X_Indices, Y_Indices  : Integer_List;
      Bunch                 : Bunch_Data);
   procedure Set_Default_Target
     (Features_List  : in out AR_Types.Attribute_List;
      Target_Columns : out String_List);
   procedure Verify_Target_Data_Type
     (Features_Dict  : AR_Types.Attribute_Dictionary_Map;
      Target_Columns : String_List);

   --  ------------------------------------------------------------------------
   --  L475
   procedure Download_Data_To_Bunch
     (ARFF_Container      : AR_Types.ARFF_Record;
      Features_List       : AR_Types.Attribute_List;
      Data_Column_Names   : String_Vector;
      Target_Column_Names : in out String_List;
      Bunch               : out Bunch_Data;
      --        Sparse       : Boolean;
      As_Frame            : As_Frame_State := As_Frame_False) is
      --        Shape        : Shape_Data) is
      use Ada.Containers;
      use AR_Types;
      use ML_Types.Integer_DLL_Package;
      use Attribute_Data_Package;
      use String_Package;
      Routine_Name       : constant String :=
                             "Openml_Ada.Download_Data_To_Bunch ";
      Feature_Index      : Positive;
      Features_Dict      : Attribute_Dictionary_Map;
      aFeature           : Attribute_Record;
--        Data_Cursor        : String_Package.Cursor := Data_Column_Names.First;
      Target_Cursor      : String_Package.Cursor := Target_Column_Names.First;
      Col_Name           : Unbounded_String;
      Col_Slice_X        : ML_Types.Integer_DL_List;
      Col_Slice_Y        : ML_Types.Integer_DL_List;
      --        Num_Missing        : Integer;
      --        Return_Type        : ARFF_Return_Type;
      X                  : Float_List_2D;
      Y                  : Integer_List;
      Nominal_Attributes : Nominal_Data_List;
      --        Frame              : Boolean := False;

   begin
      Assert (not Is_Empty (Features_List), Routine_Name &
                "called with empty Features_List.");
      Assert (Data_Column_Names.Length > 0, Routine_Name &
                "Data_Column_Names is empty.");

      --        Load_ARFF_Data.ARFF_Printing.Print_Attributes
      --          (Routine_Name & "Features_List", Features_List);

      --  L494
      for index in Features_List.First_Index .. Features_List.Last_Index loop
         aFeature := Features_List.Element (index);
         Features_Dict.Include (aFeature.Name, index);
      end loop;

--        Printing.Print_Strings (Routine_Name & "Target_Column_Names",
--                                Target_Column_Names);
      --  col_slice_y should be all nominal or all numeric
      Verify_Target_Data_Type (Features_Dict, Target_Column_Names);

      --  L499 col_slice_y =
      --        [
      --          int(features_dict[col_name]["index"])
      --          for col_name in target_columns
      --        ]
      --  target_columns is a list of attribute names
      while Has_Element (Target_Cursor) loop
         Col_Name := Element (Target_Cursor);
         Feature_Index := Features_Dict.Element (Col_Name);
         Col_Slice_Y.Append (Feature_Index);
         Next (Target_Cursor);
      end loop;

      --  L501
      for index in Data_Column_Names.First_Index .. Data_Column_Names.Last_Index loop
         Col_Name := Data_Column_Names.Element (index);
         Feature_Index := Features_Dict.Element (Col_Name);
         Col_Slice_X.Append (Feature_Index);
      end loop;

      --  L515
      --        if As_Frame then
      --           null;
      --        elsif Sparse
      --           null;
      --        else
      --           null;

      --  L522
      ARFF_Parser.Arff_Parser (ARFF_Container, Target_Column_Names,
                               Col_Slice_X, Col_Slice_Y, X, Y);
      Nominal_Attributes :=
        Parse_Nominal_Data (ARFF_Container, Target_Column_Names);

      Bunch.Data := X;
      Bunch.Target := Y;
      Bunch.As_Frame := As_Frame;
      Bunch.Categories := Nominal_Attributes;
      Bunch.Feature_Names := Data_Column_Names;
      Bunch.Target_Names := Target_Column_Names;

   end Download_Data_To_Bunch;

   --  ------------------------------------------------------------------------
   --  Target_Column : str, list or None, specifies the column name in the data
   --  to use as target. If empty, all columns are returned as data and the
   --  target is `None`. If a list of strings, all columns with these names
   --  are returned as a multi-target.
   --  L592
   procedure Fetch_Openml
     (Dataset_File_Name : String;
      Save_File_Name    : String;
      Target_Columns    : in out String_List;
--        X                 : out Float_List_2D;
--        Y                 : out Integer_List;
      --        X_Indices         : out Integer_List;
      --        Y_Indices         : out Integer_List;
      Bunch             : out Bunch_Data;
      As_Frame          : in out As_Frame_State) is
      use Ada.Directories;
      use Ada.Strings;
      use AR_Types;
      use Load_ARFF_Data;
      use ARFF_IO;
      use String_Package;
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
      Data_Columns    : String_Vector;
      --        Shape           : Shape_Data;
      --        Data_Qualities  : Qualities_Map;
   begin
      if Exists (Save_File_Name) then
         Get_OML (Save_File_Name, Bunch);
      else
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

         --  L789
         if As_Frame = As_Frame_Auto then
            if not Return_Sparse then
               As_Frame := As_Frame_True;
            end if;
         else
            As_Frame := As_Frame_False;
         end if;

         Assert (not (As_Frame = As_Frame_True and Return_Sparse),
                 Routine_Name & "cannot return dataframe with sparse data");

         --  L796
         Features_List := Get_Attributes (ARFF_Data);

         if As_Frame = As_Frame_False then
            Process_Feature (Features_List);
         end if;

         --  L929
         if Target_Columns.Is_Empty then
            Set_Default_Target (Features_List, Target_Columns);
         end if;

         --  L944
         Data_Columns :=
           Valid_Data_Column_Names (Features_List, Target_Columns);
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
                                 Target_Columns, Bunch);
         --           for index in X.First_Index .. X.Last_Index loop
         --              X_Indices.Append (index);
         --              Y_Indices.Append (index);
         --           end loop;

         if Save_File_Name'Length > 0 then
            Save_OML (Save_File_Name, Bunch);
         end if;
      end if;

   end Fetch_Openml;

   --  ------------------------------------------------------------------------

   procedure Get_OML (File_Name : String;
                      Bunch     : out Bunch_Data) is
      use Ada.Streams;
      use Stream_IO;
      Routine_Name : constant String := "Openml_Ada.Get_OML ";
      File_ID      : Stream_IO.File_Type;
      aStream      : Stream_Access;
   begin
      Put_Line (Routine_Name & "Reading OML file " & File_Name);
      Open (File_ID, In_File, File_Name);
      aStream := Stream (File_ID);
      Bunch_Data'Read (aStream, Bunch);
      Close (File_ID);
      pragma Unreferenced (File_ID);

   end Get_OML;

   --  ------------------------------------------------------------------------

   function Parse_Nominal_Data
     (Arff_Data       : AR_Types.ARFF_Record;
      Include_Columns : String_List)
      return AR_Types.Nominal_Data_List is
      --        use Ada.Containers;
      use AR_Types;
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

   procedure Process_Feature (Features_List : AR_Types.Attribute_List) is
      use AR_Types;
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
   procedure Set_Default_Target
     (Features_List  : in out AR_Types.Attribute_List;
      Target_Columns : out String_List) is
      use AR_Types;
      use Attribute_Data_Package;
--        Routine_Name     : constant String := "Openml_Ada.Set_Default_Target ";
      Feature          : Attribute_Record;
   begin
      for index in Features_List.First_Index .. Features_List.Last_Index loop
         Feature := Features_List.Element (index);
         if Feature.Is_Target then
            Target_Columns.Append (Feature.Name);
         elsif To_String (Feature.Name) = "class" then
            Target_Columns.Append (Feature.Name);
            Feature.Is_Target := True;
            Features_List.Replace_Element (index, Feature);
         end if;
      end loop;

   end Set_Default_Target;

   --  ------------------------------------------------------------------------

   procedure Save_OML (Save_File_Name : String;
                       --                         X_Indices      : Integer_List;
                       --                         Y_Indices      : Integer_List;
                       Bunch          : Bunch_Data) is
      use Ada.Streams;
      use Stream_IO;
      File_ID  : Stream_IO.File_Type;
      aStream  : Stream_Access;
   begin
      Create (File_ID, Out_File, Save_File_Name);
      aStream := Stream (File_ID);
      --        Integer_List'Write (aStream, X_Indices);
      --        Integer_List'Write (aStream, Y_Indices);
      Bunch_Data'Write (aStream, Bunch);
      Close (File_ID);
      pragma Unreferenced (File_ID);

   end Save_OML;

   --  ------------------------------------------------------------------------
   --  L699
   function Valid_Data_Column_Names
     (Features_List  : AR_Types.Attribute_List;
      Target_Columns : String_List) return String_Vector is
      --        use Ada.Text_IO.Unbounded_IO;
      use AR_Types;
      --        Routine_Name  : constant String := "Openml_Ada.Valid_Data_Column_Names ";
      Feature       : Attribute_Record;
      Feature_Name  : Unbounded_String;
      Valid_Names   : String_Vector;

      function Is_A_Target return Boolean is
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
   --  L549 Verify_Target_Data_Type verifies the data type of the y array in
   --  case there are multiple targets.
   --  Verify_Target_Data_Type throws an error for any target that does not
   --  comply with sklearn support.
   procedure Verify_Target_Data_Type
     (Features_Dict  : AR_Types.Attribute_Dictionary_Map;
      Target_Columns : String_List) is
      use Ada.Characters.Handling;
      Routine_Name  : constant String := "Openml_Ada.Verify_Target_Data_Type ";
      use String_Package;
      Curs          : Cursor := Target_Columns.First;
      Column        : Unbounded_String;
      Found_Types   : Unbounded_List;
   begin
      --        Put_Line (Routine_Name & "Target_Columns length" &
      --                    Integer'Image (Length (Target_Columns)));
      while Has_Element (Curs) loop
         Column := Element (Curs);
         Assert (Features_Dict.Contains (Column),
                 Routine_Name & "Features_Dict does not contain " &
                   To_String (Column));
         declare
            UC_Column : constant String := To_Upper (To_String (Column));
         begin
            if UC_Column = "NUMERIC" or else UC_Column = "INTEGER" or else
              UC_Column = "REAL" or else UC_Column = "NOMINAL" then
               Found_Types.Append (Column);
            end if;
         end;
         Next (Curs);
      end loop;

      Assert (Integer (Found_Types.Length) <= 1, Routine_Name &
                "Can only handle homogeneous multi-target datasets, " &
                "i.e., all targets are either numeric or categorical.");

   end Verify_Target_Data_Type;

   --  ------------------------------------------------------------------------

end Openml_Ada;
