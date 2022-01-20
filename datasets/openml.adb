--  Based on scikit-learn/sklearn/datasets _openml.py

with Ada.Assertions; use Ada.Assertions;
--  with Ada.Containers.Doubly_Linked_Lists; with Ada.Containers.Vectors;
with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;

with AWS.Client;
with AWS.Response;
with AWS.URL;

with ML_Types;

with ARFF;
with Dataset_Utilities;

package body Openml is

   --     type JSON_Item is record
   --        Name  : Unbounded_String;
   --        Value : Unbounded_String;
   --     end record;

   type ARFF_Type is (ARFF_COO, ARFF_DENSE_GEN);

      type Shape_Data is record
         Num_Samples     : Natural := 0;
         Features_Length : Natural := 0;
      end record;

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

   --     type Json_Data is record
   --        ID                       : Integer := 1;
   --        Name                     : Unbounded_String :=
   --                                     To_Unbounded_String ("anneal");
   --        Version                  : Integer := 2;
   --        Description              : Unbounded_String;
   --        Format                   : String (1 .. 4) := "ARFF";
   --        Upload_Date              : Unbounded_String;
   --        Licence                  : Unbounded_String :=
   --                                     To_Unbounded_String ("Public");
   --        URL                      : Unbounded_String :=
   --                                     To_Unbounded_String ("https:\/\/www.openml.org\/data\/v1\/download\/1\/anneal.arff");
   --        File_id                  : Integer := 1;
   --        Default_Target_Attribute : Unbounded_String := To_Unbounded_String ("class");
   --        Version_Label            : Integer := 2;
   --        Tag                      : Unbounded_String := To_Unbounded_String
   --          ("[study_1, study_41, study_7, uci]");
   --        Visibility               : Unbounded_String :=
   --                                     To_Unbounded_String ("public");
   --        Original_Data_Url        : Unbounded_String :=
   --                                     To_Unbounded_String ("http:\/\/www.openml.org\/d\/2");
   --        Status                   : Unbounded_String :=
   --                                     To_Unbounded_String ("deactivated");
   --        Processing_Date          : Unbounded_String :=
   --                                     To_Unbounded_String ("2018-10-03 21:46:33");
   --        Error                    : Unbounded_String :=
   --                                     To_Unbounded_String ("Read timed out");
   --        Md5_Checksum             : Unbounded_String;
   --     end record;

   Openml_Prefix  : constant String := "http://openml.org/";
   Search_Name    : constant String := "api/v1/json/data/list/data_name/";
   --     Data_Info      : constant String := "api/v1/json/data/";
   Data_Features  : constant String := "api/v1/json/data/features/";
   --     Data_Qualities : constant String := "api/v1/json/data/qualities/";
   Data_File      : constant String := "data/v1/download/";

   function Get_Json_Content_From_File (File_Name : String) return JSON_Value;
   function Get_Json_Content_From_Openml_Api (URL : String) return JSON_Value;
   function Get_Num_Samples (Qualities : Qualities_Map) return Integer;
   function Open_Openml_URL (Openml_Path : String) return AWS.Response.Data;
   procedure Process_Feature (Dataset_Name  : String;
                              Features_List : JSON_Array);
   procedure Process_Target (Features_List  : JSON_Array;
                             Target_Columns : out JSON_Array);
   function Split_Sparse_Columns
     (Arff_Data : ARFF.Arff_Container_Type; Include_Columns : ML_Types.String_List)
      return JSON_Array;
   function Valid_Data_Column_Names
     (Features_List, Target_Columns : JSON_Array) return JSON_Array;
   procedure Verify_Target_Data_Type (Features_Dict : JSON_Value;
                                      Target_Columns : JSON_Array);

   --  ------------------------------------------------------------------------

--     function Convert_Arff_Data_Dataframe
--       (ARFF_Container : ARFF.Arff_Container_Type; Features : JSON_Value) return JSON_Value is
--        Result : JSON_Value;
--     begin
--        return Result;
--
--     end Convert_Arff_Data_Dataframe;

   --  ------------------------------------------------------------------------
   --  A Tuple is a collection of Python objects separated by commas.
   procedure Convert_Arff_To_Data
     (ARFF : JSON_Value; Col_Slice_X , Col_Slice_Y : ML_Types.String_List;
      X, Y : out JSON_Array) is
      Arff_Data   : JSON_Array := Get (ARFF, "data");
      Arff_Data_X : JSON_Array;
      Tuple_Length : Positive;
   begin
      --  L283
      Arff_Data_X := Split_Sparse_Columns (ARFF, Col_Slice_X);
   end Convert_Arff_To_Data;

   --  ------------------------------------------------------------------------

  procedure Download_Data_To_Bunch (URL : String; Sparse, As_Frame : Boolean;
                                    Features_List  : JSON_Array;
                                    Data_Columns   : JSON_Array;
                                    Target_Columns : JSON_Array;
                                    Shape : Shape_Data) is
      Routine_Name    : constant String := "Openml.Download_Data_To_Bunch ";
      Feature_Index : Positive := Array_First (Features_List);
      Col_Name      : Positive := Array_First (Features_List);
      Features_Dict : JSON_Value;
      aFeature      : JSON_Value;
      aColumn       : JSON_Value;
      Feature_Name  : JSON_Value;
      Col_Slice_X   : JSON_Array;
      Col_Slice_Y   : JSON_Array;
      Num_Missing   : Integer;
      Return_Type   : ARFF_Type;
      Columns       : JSON_Array;
      Parsed_ARFF   : JSON_Array;

        procedure Parse_ARFF (ARFF : JSON_Array; X, Y : out JSON_Array) is
        begin
            null;
        end Parse_ARFF;
  begin
      while Array_Has_Element (Features_List, Feature_Index) loop
         aFeature := Array_Element (Features_List, Feature_Index);
         Feature_Name := Get (aFeature, "name");
         Features_Dict.Append (Feature_Name);
         Feature_Index := Array_Next (Features_List, Feature_Index);
      end loop;

      Verify_Target_Data_Type (Features_Dict, Target_Columns);

      --  L566 col_slice_y =
      --        [
      --          int(features_dict[col_name]["index"])
      --          for col_name in target_columns
      --        ]
      while Array_Has_Element (Target_Columns, Col_Name) loop
         aFeature := Array_Element (Features_List, Col_Name);
         aColumn := Get (aFeature, "index");
         Append (Col_Slice_Y, aColumn);
         Col_Name := Array_Next (Target_Columns, Col_Name);
      end loop;

      --  L566 continued
      Col_Name := Array_First (Features_List);
      while Array_Has_Element (Data_Columns, Col_Name) loop
         aFeature := Array_Element (Features_List, Col_Name);
         aColumn := Get (aFeature, "index");
         Append (Col_Slice_X, aColumn);
         Col_Name := Array_Next (Data_Columns, Col_Name);
      end loop;

      --  L568
      while Array_Has_Element (Data_Columns, Col_Name) loop
         aFeature := Array_Element (Features_List, Col_Name);
         aColumn := Get (aFeature, "index");
         Append (Col_Slice_X, aColumn);
         Col_Name := Array_Next (Data_Columns, Col_Name);
      end loop;

      --  L569
      Col_Name := Array_First (Col_Slice_Y);
      while Array_Has_Element (Col_Slice_Y, Col_Name) loop
         aFeature := Array_Element (Features_List, Col_Name);
         Num_Missing := Get (aFeature, "number_of_missing_values");
         Assert (Num_Missing >= 0,
                Routine_Name & "Target column " & " has " & " missing values."
                & "Missing values are not supported for target columns.");
         Col_Name := Array_Next (Col_Slice_Y, Col_Name);
      end loop;

        --  L582
      if Sparse then
         Return_Type := ARFF_COO;
      else
         Return_Type := ARFF_DENSE_GEN;
      end if;

      --  L593
      if As_Frame then
         Columns := Data_Columns;
         Col_Name := Array_First (Target_Columns);
         while Array_Has_Element (Target_Columns, Col_Name) loop
            Append (Columns, Array_Element (Target_Columns, Col_Name));
            Col_Name := Array_Next (Target_Columns, Col_Name);
         end loop;

      else
         null;
      end if;

  end Download_Data_To_Bunch;

   --  ------------------------------------------------------------------------

procedure Fetch_Openml (Dataset_Name  : String; Version : String := "";
                           Data_Id       : in out Integer;
                           Target_Column : String := "default-target";
                           --                             Return_X_Y    : Boolean := False;
                           As_Frame      : String := "false") is
      use Dataset_Utilities;
      Routine_Name    : constant String := "Openml.Fetch_Openml ";
      Dataset_Name_LC : constant String := To_Lower_Case (Dataset_Name);
      Data_Url        : constant String := Data_File & "file_id";
      Data_Info       : JSON_Value;
      JSON_Data_Id    : JSON_Value;
      Description     : JSON_Value;
      Return_Sparse   : Boolean := False;
      Data_Format     : JSON_Value;
      Features_List   : JSON_Array;
      Ignore          : JSON_Value;
      Target_Columns  : JSON_Array;
      Data_Columns    : JSON_Array;
      Shape           : Shape_Data;
      Data_Qualities  : Qualities_Map;
   begin
      --  L862
      Data_Info := Get_Data_Info_By_Name (Dataset_Name_LC, Version);
      JSON_Data_Id := Get (Data_Info, "data_id");
      Data_Id := Integer'Value (Get (JSON_Data_Id));

      --  L877
      Description := Get_Data_Description_By_ID (Data_Id);
      Data_Format := Get (Description, "format");
      declare
         Format : String := Get (Data_Format);
      begin
         Format := To_Lower_Case (Format);
         Return_Sparse := Format = "sparse_arff";
      end;

      --  L903
      if As_Frame = "auto" then
         Return_Sparse := not Return_Sparse;
      end if;

      Assert (not (As_Frame = "true" and Return_Sparse),
              Routine_Name & "cannot return dataframe with sparse data");

      Put_Line (Routine_Name & "As_Frame: " & As_Frame);
      --  L910
      Features_List := Get_Data_Features (Data_ID);
      if As_Frame = "false" then
         Process_Feature (Dataset_Name, Features_List);
      end if;

      --  L922
      if Target_Column = "default-target" then
         Process_Target (Features_List, Target_Columns);
      end if;

      --  L944
      Data_Columns := Valid_Data_Column_Names (Features_List, Target_Columns);

      --  L948
      if not Return_Sparse then
         Data_Qualities := Get_Data_Qualities (Data_Id, Dataset_Name);
         if Get_Num_Samples (Data_Qualities) > -1 then
            Shape := (Get_Num_Samples (Data_Qualities), Length (Features_List));
         end if;
      end if;

      --  L970
      --        if Return_X_Y then
      --           null;
      --        end if;

   end Fetch_Openml;

   --  ------------------------------------------------------------------------

   function Get_Data_Description_By_ID
     (Data_ID : Integer; File_Name : String := "") return JSON_Value is
      use Ada.Strings;
      Routine_Name : constant String := "Openml.Get_Data_Description_By_ID ";
      URL          : constant String := Data_Features &
                       Fixed.Trim (Integer'Image (Data_ID), Both);
      URL_Object   : AWS.URL.Object;
      Data_Desc    : JSON_Value;
      --        Value_Type   : JSON_Value_Type;
   begin
      --  URL.Parse parses an URL and returns an Object representing this URL.
      --  It is then possible to extract each part of the URL with other AWS.URL
      --  services.
      URL_Object := AWS.URL.Parse (URL);
      Assert (AWS.URL.Is_Valid (URL_Object), Routine_Name &
                "object returned by URL " & URL & "is invalid");

      if File_Name = "" then
         Data_Desc := Get_Json_Content_From_Openml_Api (URL);
      else
         Data_Desc := Get_Json_Content_From_File (File_Name);
      end if;

      if Has_Field (Data_Desc, "description") then
         Data_Desc := Get (Data_Desc, "description");
      else
         Put_Line (Routine_Name & "Data_Desc is not a data_set_description");
      end if;

      return Data_Desc;

   end Get_Data_Description_By_ID;

   --  ------------------------------------------------------------------------

   function Get_Data_Features (Data_ID   : Integer;
                               File_Name : String := "") return JSON_Array is
      use Ada.Strings;
      Routine_Name  : constant String := "Openml.Get_Data_Features ";
      Json_Data     : JSON_Value := Create_Object;
      Features      : JSON_Value := Create_Object;
      Feature       : JSON_Value := Create_Object;
      Feature_Array : JSON_Array;
   begin
      if File_Name = "" then
         Json_Data := Get_Json_Content_From_Openml_Api
           (Data_Features & Fixed.Trim (Integer'Image (Data_ID), Both));
      else
         Json_Data := Get_Json_Content_From_File (File_Name);
      end if;

      Assert (Has_Field (Json_Data, "data_features"), Routine_Name &
                "data_features is not a Json_Data field.");
      Features := Get (Json_Data, "data_features");

      Assert (Has_Field (Features, "feature"), Routine_Name &
                "data_features is not a Json_Data field.");
      Feature := Get (Features, "feature");
      Feature_Array := Get (Feature);

      return Feature_Array;

   end Get_Data_Features;

   --  ------------------------------------------------------------------------

   function Get_Data_Info_By_Name (Dataset_Name      : String;
                                   Version           : String := "";
                                   Active            : Boolean := False;
                                   File_Name         : String := "")
                                   return JSON_Value is
      --        Routine_Name   : constant String := "Openml.Get_Data_Info_By_Name ";
      Openml_Path    : Unbounded_String :=
                         To_Unbounded_String (Search_Name);
      Json_Data      : JSON_Value;
   begin
      if File_Name = "" then
         if Active then
            Openml_Path := Openml_Path & "limit/2/status/active/";
         else
            Openml_Path := Openml_Path & Dataset_Name & "/limit/2/" &
              "data_version/" & Version;
         end if;

         Json_Data := Get_Json_Content_From_Openml_Api
           (To_String (Openml_Path));
      else
         Json_Data := Get_Json_Content_From_File (File_Name);
      end if;

      return Json_Data;

   end Get_Data_Info_By_Name;

   --  ------------------------------------------------------------------------

   function Get_Data_Qualities (Data_ID : Integer; Dataset_Name : String := "")
                                return Qualities_Map is
      use Ada.Strings;
      --        Routine_Name  : constant String := "Openml.Get_Data_Qualities ";
      Json_Data     : JSON_Value;
      Qualities     : JSON_Value;
      Quality_Array : Qualities_Map;

      procedure Get_Quality (Name : Utf8_String; Value : JSON_Value) is
         use ML_Qualities_Package;
         Array_Quality : Boolean;
         Bool_Quality  : Boolean;
         Float_Quality : Float;
         Int_Quality   : Integer;
         Quality       : constant JSON_Value := Create_Object;
         Null_Value    : Boolean := False;
      begin
         case Kind (Value) is
            when JSON_Array_Type =>
               Array_Quality := Get (Value);
               Quality.Set_Field (Name, Array_Quality);
            when JSON_Boolean_Type =>
               Bool_Quality := Get (Value);
               Quality.Set_Field (Name, Bool_Quality);
            when JSON_Float_Type =>
               Float_Quality := Get (Value);
               Quality.Set_Field (Name, Float_Quality);
            when JSON_Int_Type =>
               Int_Quality := Get (Value);
               Quality.Set_Field (Name, Int_Quality);
            when JSON_Null_Type => Null_Value := True;
            when JSON_Object_Type =>
               Quality.Set_Field (Name, Value);
            when JSON_String_Type =>
               declare
                  String_Quality : constant String := Get (Value);
               begin
                  Quality.Set_Field (Name, String_Quality);
               end;
         end case;

         if not Null_Value then
            Quality_Array.Include (To_Unbounded_String (Name), Value);
         end if;

      end Get_Quality;

   begin
      if Dataset_Name = "" then
         Json_Data := Get_Json_Content_From_Openml_Api
           (Data_Features & Fixed.Trim (Integer'Image (Data_ID), Both));
      else
         Json_Data := Get_Json_Content_From_File (Dataset_Name);
      end if;

      if Has_Field (Json_Data, "qualities") then
         Qualities := Get (Json_Data, "qualities");
         Map_JSON_Object (Qualities, Get_Quality'access);
      end if;

      return Quality_Array;

   end Get_Data_Qualities;

   --  ------------------------------------------------------------------------

   function Get_Json_Content_From_File (File_Name : String) return JSON_Value is
      --        Routine_Name   : constant String :=
      --                           "Openml.Get_Json_Content_From_File ";
      Name           : constant String := File_Name & ".json";
      File           : File_Type;
      JSON_Data      : Unbounded_String;
      JSON_Main_Node : JSON_Value := Create;
   begin
      Open (File, In_File, Name);
      JSON_Data := To_Unbounded_String (Get_Line (File));
      Close (File);

      JSON_Main_Node := Read (JSON_Data, Filename => "");
      return JSON_Main_Node;

   end Get_Json_Content_From_File;

   --  ------------------------------------------------------------------------

   function Get_Json_Content_From_Openml_Api (URL : String)
                                              return JSON_Value is
      Routine_Name      : constant String :=
                            "Openml.Get_Json_Content_From_Openml_Api ";
      AWS_Reply         : constant Aws.Response.Data := Open_Openml_URL (URL);
      AWS_Response      : constant Unbounded_String :=
                            AWS.Response.Message_Body (AWS_Reply);
      JSON_Main_Node    : JSON_Value := Create;
   begin
      Put_Line (Routine_Name & "URL:");
      Put_Line (URL);
      New_Line;
      Put_Line (Routine_Name & "AWS_Response:");
      Put_Line (To_String (AWS_Response));
      JSON_Main_Node := GNATCOLL.JSON.Read
        (Strm => Unbounded_String'(AWS_Response),
         Filename => "");

      return JSON_Main_Node;

   end Get_Json_Content_From_Openml_Api;

   --  ------------------------------------------------------------------------

   function Get_Num_Samples (Qualities : Qualities_Map) return Integer is
      use ML_Qualities_Package;
      --        Routine_Name  : constant String := "Openml.Get_Num_Samples ";
      Curs          : Cursor := Qualities.First;
      Quality       : JSON_Value;
      Num_Samples   : Integer := -1;

      procedure Get_Num_Instances (Name : Utf8_String; Value : JSON_Value) is
         Num_Instances : Float := 0.0;
      begin
         if Name = "NumberOfInstances" then
            Num_Instances := Get (Value);
            Num_Samples := Integer (Num_Instances);
         end if;
      end Get_Num_Instances;

      procedure Get_Qual (Name : Utf8_String; Value : JSON_Value) is
         Name_Quality : constant JSON_Value := Create_Object;
      begin
         if Name = "name" and then Kind (Value) = JSON_String_Type then
            declare
               String_Value : constant String := Get (Value);
            begin
               if String_Value = "value" then
                  Name_Quality.Set_Field (Name, Value);
               end if;
               Map_JSON_Object (Name_Quality, Get_Num_Instances'access);
            end;
         end if;

      end Get_Qual;

   begin
      while Has_Element (Curs) loop
         Quality := Element (Curs);
         Map_JSON_Object (Quality, Get_Qual'access);
         Next (Curs);
      end loop;

      return Num_Samples;

   end Get_Num_Samples;

   --  ------------------------------------------------------------------------

   procedure Load_Arff_Response (URL : String) is
      Response : AWS.Response.Data := Open_Openml_URL (URL);
   begin
      null;
   end Load_Arff_Response;

   --  ------------------------------------------------------------------------

   function Open_Openml_URL (Openml_Path : String) return AWS.Response.Data is
      use AWS;
      Routine_Name   : constant String := "Openml.Open_Openml_URL ";
      URL_Object     : constant AWS.URL.Object :=
                         AWS.URL.Parse (Openml_Prefix & Openml_Path);
      --        Headers        : Client.Header_List;
      AWS_Reply      : Response.Data;
   begin
      Assert (AWS.URL.Is_Valid (URL_Object),
              "Get_Data_Description_By_ID object returned by URL " &
                Openml_Prefix & Openml_Path & "is invalid");
      --        Headers.Add ("application", "json");
      Put_Line (Routine_Name & "aws url:");
      Put_Line (AWS.URL.URL (URL_Object));
      New_Line;

      --        AWS_Reply := Get (AWS.URL.URL (URL_Object));
      AWS_Reply := AWS.Client.Get
        ("http://www.openml.org/api/v1/json/data/list/data_name/mnist_784/limit/2/data_version/1",
         Follow_Redirection => False);
      Put_Line (Routine_Name & "data type: " &
                  Response.Content_Type (AWS_Reply));
      New_Line;
      --        JSON_Main_Node := Read
      --          (Strm => Unbounded_String'(Aws.Response.Message_Body(Aws_Reply)),
      --           Filename => "");
      --        Main_Node := Get (JSON_Main_Node);
      --        aValue := Get (Main_Node, 1);
      --        Put_Line (Routine_Name & "data type: " &
      --                    JSON_Value_Type'Image (Kind (aValue)));

      Put_Line  (Routine_Name & "Response ");
      Put_Line  (To_String (AWS.Response.Message_Body (AWS_Reply)));
      Put_Line (Routine_Name & "done");
      return AWS_Reply;

   end Open_Openml_URL;

   --  ------------------------------------------------------------------------

   procedure Process_Feature (Dataset_Name  : String;
                              Features_List : JSON_Array) is
      Routine_Name   : constant String := "Openml.Process_Feature ";
      Feature_Index  : Positive;
      Feature_Name   : JSON_Value;
      Ignore         : JSON_Value;
      Is_Row_ID      : JSON_Value;
      Data_Type_Item : JSON_Value;
   begin
      Feature_Index := Array_First (Features_List);
      while Array_Has_Element (Features_List, Feature_Index) loop
         Feature_Name := Array_Element (Features_List, Feature_Index);
         Put_Line (Routine_Name & "Feature_Name JSON type: " &
                     JSON_Value_Type'Image (Kind (Feature_Name)));
         Ignore := Get (Feature_Name, "is_ignore");
         Is_Row_ID := Get (Feature_Name, "is_row_identifier");
         declare
            Ignore_Status : constant String := Get (Ignore);
            Row_ID_Status : constant String := Get (Is_Row_ID);
         begin
            if Ignore_Status /= "true" and Row_ID_Status /= "true" then
               Data_Type_Item := Get (Feature_Name, "data_type");
               declare
                  Data_Type : constant String := Get (Data_Type_Item);
               begin
                  Assert (Data_Type /= "string", Routine_Name & Dataset_Name
                          & " invalid as STRING attributes are not " &
                            "supported for array representation. " &
                            "Try as_frame=True");
               end;
            end if;
         end;
         Feature_Index := Array_Next (Features_List, Feature_Index);
      end loop;

   end Process_Feature;

   --  ------------------------------------------------------------------------

   procedure Process_Target (Features_List  : JSON_Array;
                             Target_Columns : out JSON_Array) is
      Feature_Index : Positive;
      Feature_Name  : JSON_Value;
      Target        : JSON_Value;
   begin
      Feature_Index := Array_First (Target_Columns);
      while Array_Has_Element (Features_List, Feature_Index) loop
         Feature_Name := Array_Element (Features_List, Feature_Index);
         Target := Get (Feature_Name, "is_target");
         declare
            Target_Status : constant String := Get (Target);
         begin
            if Target_Status = "true" then
               Target_Columns := Get (Feature_Name, "name");
            end if;
         end;
         Feature_Index := Array_Next (Features_List, Feature_Index);
      end loop;

   end Process_Target;

   --  ------------------------------------------------------------------------
   --  L184
   function Split_Sparse_Columns
     (Arff_Data : ARFF.Arff_Container_Type; Include_Columns : ML_Types.String_List)
      return JSON_Array is
      Arff_Data_New : ARFF.Arff_Sparse_Data_Type := Arff_Data;
   begin
      return Arff_Data_New;

   end Split_Sparse_Columns;

   --  ------------------------------------------------------------------------

   function Valid_Data_Column_Names
     (Features_List, Target_Columns : JSON_Array) return JSON_Array is
      --        Routine_Name  : constant String := "Openml.Valid_Data_Column_Names ";
      Feature_Index : Positive;
      Feature_Name  : JSON_Value;
      Name_Index    : Positive;
      Ignore        : JSON_Value;
      Is_Row_ID     : JSON_Value;
      Found         : Boolean;
      Column_Names  : JSON_Array;
   begin
      Feature_Index := Array_First (Target_Columns);
      while Array_Has_Element (Features_List, Feature_Index) loop
         Feature_Name := Array_Element (Features_List, Feature_Index);
         Found := False;
         Name_Index := Array_First (Target_Columns);
         while Array_Has_Element (Target_Columns, Name_Index) and
           not Found loop
            Ignore := Get (Feature_Name, "is_ignore");
            Is_Row_ID := Get (Feature_Name, "is_row_identifier");
            declare
               Ignore_Status : constant String := Get (Ignore);
               Row_ID_Status : constant String := Get (Is_Row_ID);
            begin
               Found :=
                 Feature_Name = Array_Element (Target_Columns, Name_Index)
                 and Ignore_Status /= "true" and Row_ID_Status /= "true";
            end;
            Name_Index := Array_Next (Target_Columns, Name_Index);
         end loop;

         Feature_Index := Array_Next (Features_List, Feature_Index);
      end loop;

      return Column_Names;

   end Valid_Data_Column_Names;

   --  ------------------------------------------------------------------------

   procedure Verify_Target_Data_Type (Features_Dict : JSON_Value;
                                      Target_Columns : JSON_Array) is
   begin
    null;
   end Verify_Target_Data_Type;

   --  ------------------------------------------------------------------------

end Openml;
