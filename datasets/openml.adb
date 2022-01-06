--  Based on scikit-learn/sklearn/datasets _openml.py

with Ada.Assertions; use Ada.Assertions;
with Ada.Characters.Handling;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Ordered_Maps;
--  with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.JSON;

with AWS.Client;
with AWS.Response;
--  with AWS.Status;
with AWS.URL;

package body Openml is

   --     type JSON_Item is record
   --        Name  : Unbounded_String;
   --        Value : Unbounded_String;
   --     end record;

   use GNATCOLL.JSON;
   --     package Pair_Settings_Vector_Package is new
   --       Ada.Containers.Vectors (Natural, JSON_Item);
   --     subtype Pair_Settings_Vector is Pair_Settings_Vector_Package.Vector;

   package ML_Names_Package is new
     Ada.Containers.Doubly_Linked_Lists (Unbounded_String);
   subtype Names_List is ML_Names_Package.List;

   package ML_Features_Package is new
     Ada.Containers.Ordered_Maps (Unbounded_String, Unbounded_String);
   subtype Features_Map is ML_Features_Package.Map;

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

   Openml_Prefix  : constant String := "https://openml.org/";
   Search_Name    : constant String := "api/v1/json/data/list/data_name/";
   --     Data_Info      : constant String := "api/v1/json/data/";
   Data_Features  : constant String := "api/v1/json/data/features/";
   --     Data_Qualities : constant String := "api/v1/json/data/qualities/";
   --     Data_File      : constant String := "data/v1/download/";

   function Get_Data_Description_By_ID (Data_ID : Integer) return JSON_Value;
   function Get_Data_Features (Data_ID : Integer) return JSON_Value;
   function Get_Data_Info_By_Name (Name   : String; Version : Integer;
                                   Active : Boolean := False)
                                   return JSON_Value;
   function Get_Json_Content_From_Openml_Api (URL : String) return JSON_Array;
   --     function Valid_Data_Column_Names (Features_List : Features_Map)
   --                                       return Names_List;
   --  ------------------------------------------------------------------------

   procedure Fetch_Openml (Dataset_Name  : String; Version : Integer;
                           Data_Id       : in out Integer;
                           Target_Column : String := "default-target";
                           Return_X_Y    : Boolean := False;
                           As_Frame      : String := "false") is
      use Ada.Characters.Handling;
      Routine_Name    : constant String := "Openml.Fetch_Openml ";
      Dataset_Name_LC : constant String := To_Lower (Dataset_Name);
      Data_Info       : JSON_Value;
      JSON_Data_Id    : JSON_Value;
      Description     : JSON_Value;
      Data_Format     : JSON_Value;
      Data_Type       : JSON_Value;
      Features_List   : JSON_Value;
      Return_Sparse   : Boolean := False;

      procedure Process_Feature (Name : Utf8_String; Value : JSON_Value) is
      begin
         if Name /= "is_ignore" and Name /= "is_row_identifier" then
            if Name = "data_type" then
               Data_Type := Get (Features_List, Name);
               Assert (Kind (Data_Type) /= JSON_String_Type,
                       Routine_Name & ".Process_Feature" &
                         "STRING attributes are not supported for " &
                         "array representation. Try as_frame=True");
            end if;
         end if;
      end Process_Feature;

   begin
      Data_Info := Get_Data_Info_By_Name (Dataset_Name_LC, Version);
      JSON_Data_Id := Get (Data_Info, "did");
      Data_Id := Get (JSON_Data_Id);

      Description := Get_Data_Description_By_ID (Data_Id);
      Data_Format := Get (Description, "format");
      declare
         Format : String := Get (Data_Format);
      begin
         Format := To_Lower (Format);
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

      if Target_Column = "default-target" then
         null;
      end if;

   end Fetch_Openml;

   --  ------------------------------------------------------------------------

   function Get_Data_Description_By_ID (Data_ID : Integer) return JSON_Value is
      URL          : constant String := Data_Features & Integer'Image (Data_ID);
      URL_Object   : AWS.URL.Object;
      Json_Content : JSON_Array;
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
      Json_Content := Get_Json_Content_From_Openml_Api (URL);
      Data_Desc := Get (Json_Content, 1);
      --        Value_Type := Kind (Data_Desc);

      if Has_Field (Data_Desc, "data_set_description") then
         Data_Desc := Get (Data_Desc, "data_set_description");
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
      Data     : constant JSON_Array := Get_Json_Content_From_Openml_Api (URL);
      Features : JSON_Value;
   begin
      null;

      return Features;

   end Get_Data_Features;

   --  ------------------------------------------------------------------------

   function Get_Data_Info_By_Name (Name   : String; Version : Integer;
                                   Active : Boolean := False)
                                   return JSON_Value is
      URL          : Unbounded_String :=
                       To_Unbounded_String (Search_Name & Name);
      URL_Object   : AWS.URL.Object;
      Json_Data    : JSON_Array;
      Content      : JSON_Value;
      Data         : JSON_Value;
      Data_Set     : JSON_Value;
   begin
      if Active then
         URL := URL & "/status/active/";
      else
         URL := URL & "/data_version/" & Name &
           Integer'Image (Version) & "/limit/2";
      end if;

      declare
         URL_S : constant String := To_String (URL);
      begin
         URL_Object := AWS.URL.Parse (URL_S);
         Assert (AWS.URL.Is_Valid (URL_Object),
                 "Get_Data_Description_By_ID object returned by URL " &
                   URL_S & "is invalid");
         Json_Data := Get_Json_Content_From_Openml_Api (URL_S);
      end;

      Content := Get (Json_Data, 1);
      Data := Get (Content, "data");
      Data_Set := Get (Data, "dataset");

      return Data_Set;

   end Get_Data_Info_By_Name;

   --  ------------------------------------------------------------------------
   --  Based on
   --  https://comp.lang.ada.narkive.com/kChpMHJq/gnatcoll-json-parsing
   --  JSON data is in name/value pairs
   --  JSON data pairs are separated by commas
   --  Curly braces hold objects
   --  Square brackets hold arrays
   --  The data type of a JSON value must be:
   --  a string, a number, an object, an array, a boolean or null.
   function Get_Json_Content_From_Openml_Api (URL : String) return JSON_Array is
      use AWS.Client;
      Data              : Json_Data;
      JSON_Message      : constant String :=
                            AWS.Response.Message_Body (Get (URL));
      JSON_Main_Node    : JSON_Value := Create;
      JSON_Result_Array : JSON_Array := Empty_Array;
   begin
      JSON_Main_Node := Read (JSON_Message);
      JSON_Result_Array := Get (JSON_Main_Node);

      return JSON_Result_Array;

   end Get_Json_Content_From_Openml_Api;

   --  ------------------------------------------------------------------------

   function Valid_Data_Column_Names (Features_List : Features_Map)
                                     return Names_List is
      Names : Names_List;
   begin
      return Names;

   end Valid_Data_Column_Names;

   --  ------------------------------------------------------------------------

end Openml;
