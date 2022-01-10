--  Based on scikit-learn/sklearn/datasets _openml.py

with Ada.Assertions; use Ada.Assertions;
--  with Ada.Containers.Doubly_Linked_Lists;
--  with Ada.Containers.Ordered_Maps;
--  with Ada.Containers.Vectors;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with AWS.Client;
--  with AWS.Resources;
with AWS.Response;
--  with AWS.Status;
with AWS.URL;

with Dataset_Utilities;

package body Openml is

   --     type JSON_Item is record
   --        Name  : Unbounded_String;
   --        Value : Unbounded_String;
   --     end record;

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

   --     package ML_Qualities_Package is new
   --       Ada.Containers.Ordered_Maps (Unbounded_String, Unbounded_String);
   --     subtype Qualities_Map is ML_Qualities_Package.Map;

   type Json_Data is record
      ID                       : Integer := 1;
      Name                     : Unbounded_String :=
                                   To_Unbounded_String ("anneal");
      Version                  : Integer := 2;
      Description              : Unbounded_String;
      Format                   : String (1 .. 4) := "ARFF";
      Upload_Date              : Unbounded_String;
      Licence                  : Unbounded_String :=
                                   To_Unbounded_String ("Public");
      URL                      : Unbounded_String :=
                                   To_Unbounded_String ("https:\/\/www.openml.org\/data\/v1\/download\/1\/anneal.arff");
      File_id                  : Integer := 1;
      Default_Target_Attribute : Unbounded_String := To_Unbounded_String ("class");
      Version_Label            : Integer := 2;
      Tag                      : Unbounded_String := To_Unbounded_String
        ("[study_1, study_41, study_7, uci]");
      Visibility               : Unbounded_String :=
                                   To_Unbounded_String ("public");
      Original_Data_Url        : Unbounded_String :=
                                   To_Unbounded_String ("http:\/\/www.openml.org\/d\/2");
      Status                   : Unbounded_String :=
                                   To_Unbounded_String ("deactivated");
      Processing_Date          : Unbounded_String :=
                                   To_Unbounded_String ("2018-10-03 21:46:33");
      Error                    : Unbounded_String :=
                                   To_Unbounded_String ("Read timed out");
      Md5_Checksum             : Unbounded_String;
   end record;

   Openml_Prefix  : constant String := "http://openml.org/";
   Search_Name    : constant String := "api/v1/json/data/list/data_name/";
   --     Data_Info      : constant String := "api/v1/json/data/";
   Data_Features  : constant String := "api/v1/json/data/features/";
   --     Data_Qualities : constant String := "api/v1/json/data/qualities/";
   --     Data_File      : constant String := "data/v1/download/";

   function Get_Data_Features (Data_ID : Integer) return JSON_Value;
   function Get_Json_Content_From_File (File_Name : String) return JSON_Value;
   function Get_Json_Content_From_Openml_Api (URL : String) return JSON_Value;
   function Open_Openml_URL (Openml_Path : String) return AWS.Response.Data;
   function Valid_Data_Column_Names
     (Features_List, Target_Columns : JSON_Value) return JSON_Array;

   --  ------------------------------------------------------------------------

   procedure Fetch_Openml (Dataset_Name  : String; Version : String := "";
                           Data_Id       : in out Integer;
                           Target_Column : String := "default-target";
                           Return_X_Y    : Boolean := False;
                           As_Frame      : String := "false") is
      use Dataset_Utilities;
      Routine_Name    : constant String := "Openml.Fetch_Openml ";
      Dataset_Name_LC : constant String := To_Lowercase (Dataset_Name);
      Data_Info       : JSON_Value;
      JSON_Data_Id    : JSON_Value;
      Description     : JSON_Value;
      Return_Sparse   : Boolean := False;
      Data_Format     : JSON_Value;
      Features_List   : JSON_Value;
      Feature_Name    : JSON_Value := Create_Object;
      Target_Columns  : JSON_Array;
      Data_Columns    : JSON_Array;

      procedure Process_Feature (Name : Utf8_String; Value : JSON_Value) is
      begin
         if Name /= "is_ignore" and Name /= "is_row_identifier" then
            if Name = "data_type" then
               Assert (Kind (Value) /= JSON_String_Type,
                       Routine_Name & ".Process_Feature" &
                         "STRING attributes are not supported for " &
                         "array representation. Try as_frame=True");
            end if;
         end if;
      end Process_Feature;

      procedure Process_Target (Name : Utf8_String; Value : JSON_Value) is
         True_Val     : constant UTF8_String := "true";
      begin
         if Name = "is_target" and then Get (Value, Name) = True_Val then
            Append (Target_Columns, Feature_Name);
         end if;
      end Process_Target;

   begin
      Data_Info := Get_Data_Info_By_Name (Dataset_Name_LC, Version);
      JSON_Data_Id := Get (Data_Info, "data_id");
      Data_Id := Integer'Value (Get (JSON_Data_Id));

      Description := Get_Data_Description_By_ID (Data_Id);
      Data_Format := Get (Description, "format");
      declare
         Format : String := Get (Data_Format);
      begin
         Format := To_Lowercase (Format);
         Return_Sparse := Format = "sparse_arff";
      end;

      if As_Frame = "auto" then
         Return_Sparse := not Return_Sparse;
      end if;

      Assert (not (As_Frame = "true" and Return_Sparse),
              Routine_Name & "cannot return dataframe with sparse data");

      Features_List := Get_Data_Features (Data_ID);
      if As_Frame = "false" then
         Map_JSON_Object (Features_List, Process_Feature'Access);
      end if;

      Feature_Name := Get (Features_List, "name");
      if Target_Column = "default-target" then
         Map_JSON_Object (Features_List, Process_Target'Access);
      end if;

      Data_Columns := Valid_Data_Column_Names
        (Features_List, Get (Target_Columns, 1));

   end Fetch_Openml;

   --  ------------------------------------------------------------------------

   function Get_Data_Description_By_ID
     (Data_ID : Integer; File_Name : String := "") return JSON_Value is
      use Ada.Strings;
      URL          : constant String := Data_Features &
                       Fixed.Trim (Integer'Image (Data_ID), Both);
      URL_Object   : AWS.URL.Object;
      Data_Desc    : JSON_Value;
      --        Value_Type   : JSON_Value_Type;
   begin
      --  URL.Parse parses an URL and returns an Object representing this URL.
      --  It is then possible to extract each part of the URL with other
      --  AWS.URL services.
      URL_Object := AWS.URL.Parse (URL);
      Assert (AWS.URL.Is_Valid (URL_Object),
              "Get_Data_Description_By_ID object returned by URL " &
                URL & "is invalid");
      if File_Name = "" then
         Data_Desc := Get_Json_Content_From_Openml_Api (URL);
      else
         Data_Desc := Get_Json_Content_From_File (File_Name);
      end if;

      if Has_Field (Data_Desc, "description") then
         Data_Desc := Get (Data_Desc, "description");
      else
         Put_Line ("Openml.Get_Data_Description_By_ID error, " &
                     "Json_Content is not a data_set_description");
      end if;

      return Data_Desc;

   end Get_Data_Description_By_ID;

   --  ------------------------------------------------------------------------

   function Get_Data_Features (Data_ID : Integer) return JSON_Value is
      URL      : constant String :=
                   Openml_Prefix & Data_Features & Integer'Image (Data_ID);
      --        Data     : constant JSON_Array :=
      Data     : constant JSON_Value :=
                   Get_Json_Content_From_Openml_Api (URL);
      Features : JSON_Value;
   begin
      null;

      return Features;

   end Get_Data_Features;

   --  ------------------------------------------------------------------------

   function Get_Data_Info_By_Name (Dataset_Name      : String;
                                   Version           : String := "";
                                   Active            : Boolean := False;
                                   File_Name         : String := "")
                                   return JSON_Value is
      Routine_Name   : constant String := "Openml.Get_Data_Info_By_Name ";
      Openml_Path    : Unbounded_String :=
                         To_Unbounded_String (Search_Name);
      ML_Stream      : AWS.Response.Data;
      Json_Data      : JSON_Value;
      Content        : JSON_Value;
      Data           : JSON_Value;
      Data_Set       : JSON_Value;
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

   function Get_Json_Content_From_File (File_Name : String) return JSON_Value is
      Routine_Name   : constant String :=
                         "Openml.Get_Json_Content_From_File ";
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
      Data              : Json_Data;
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

   function Open_Openml_URL (Openml_Path : String) return AWS.Response.Data is
      use AWS;
      Routine_Name   : constant String := "Openml.Open_Openml_URL ";
      URL_Object     : constant AWS.URL.Object :=
                         AWS.URL.Parse (Openml_Prefix & Openml_Path);
      --        Headers        : Client.Header_List;
      AWS_Reply      : Response.Data;
      JSON_Main_Node : JSON_Value := Create;
      Main_Node      : JSON_Array;
      AWS_Response   : Unbounded_String;
      aValue         : JSON_Value := Create;
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

   function Valid_Data_Column_Names
     (Features_List, Target_Columns : JSON_Value) return JSON_Array is
      Column_Names : JSON_Array;

      procedure Process_Names (Name : Utf8_String; Value : JSON_Value) is
         Column_Name : JSON_Value := Create_Object;
      begin
         if Name = "name" and then not Has_Field (Target_Columns, "name") then
            Column_Name.Set_Field (Name, Value);
            Append (Column_Names, Column_Name);
         end if;
      end Process_Names;

   begin
      Map_JSON_Object (Features_List, Process_Names'Access);

      return Column_Names;

   end Valid_Data_Column_Names;

   --  ------------------------------------------------------------------------

end Openml;
