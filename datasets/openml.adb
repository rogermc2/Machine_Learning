--  Based on scikit-learn/sklearn/datasets _openml.py

with Ada.Assertions; use Ada.Assertions;
with Ada.Characters.Handling;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.JSON;

with AWS.Client;
--  with AWS.Default;
--  with AWS.Headers;
--  with AWS.Resources;
with AWS.Response;
--  with AWS.Messages;
--  with AWS.MIME;
--  with AWS.Net.SSL.Certificate;
--  with AWS.Status;
with AWS.URL;
--  with AWS.Utils;

package body Openml is

   use GNATCOLL.JSON;
   package Pair_Settings_Vector_Package is new
     Ada.Containers.Vectors (Natural, JSON_Value);
   subtype Pair_Settings_Vector is Pair_Settings_Vector_Package.Vector;

   package ML_Names_Package is new
     Ada.Containers.Doubly_Linked_Lists (Unbounded_String);
   subtype Names_List is ML_Names_Package.List;

   package ML_Features_Package is new
     Ada.Containers.Ordered_Maps (Unbounded_String, Unbounded_String);
   subtype Features_Map is ML_Features_Package.Map;

   package ML_Qualities_Package is new
     Ada.Containers.Ordered_Maps (Unbounded_String, Unbounded_String);
   subtype Qualities_Map is ML_Qualities_Package.Map;

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
   Search_Name    : constant String := "api/v1/json/data/list/data_name/{}/limit/2";
   Data_Info      : constant String := "api/v1/json/data/{}";
   Data_Features  : constant String := "api/v1/json/data/features/";
   Data_Qualities : constant String := "api/v1/json/data/qualities/";
   Data_File      : constant String := "data/v1/download/";

   function Get_Data_Description_By_ID (Data_ID : Integer)
                                         return Unbounded_String;
   function Get_Data_Features (Data_ID : Integer) return Features_Map;
   function Get_Data_Info_By_Name (Name : String; Version : Integer)
                                    return Json_Data;
   function Get_Json_Content_From_Openml_Api (URL : String) return Json_Data;
   function Valid_Data_Column_Names (Features_List : Features_Map)
                                      return Names_List;
   --  ------------------------------------------------------------------------

   procedure Fetch_Openml (Dataset_Name : String; Version : Integer;
                           Data_Id      : in out Integer;
                           Return_X_Y   : Boolean := False) is
      use Ada.Characters.Handling;
      Name_LC       : constant String := To_Lower (Dataset_Name);
      Data_Info     : Json_Data;
      Description   : Unbounded_String;
      Features_List : Features_Map;
      Data          : AWS.Response.Data;
      URL_Object    : AWS.URL.Object;
      Return_Sparse : Boolean := False;
   begin
      Data_Info := Get_Data_Info_By_Name (Dataset_Name, Version);
      Data_Id := Data_Info.ID;
      Description := Get_Data_Description_By_ID (Data_Id);
      Features_List := Get_Data_Features (Data_ID);

      Data := AWS.Client.Get
        (URL => "http://perso.wanadoo.fr/pascal.obry/contrib.html");
   end ;

   --  ------------------------------------------------------------------------

   function Get_Data_Description_By_ID (Data_ID : Integer)
                                         return Unbounded_String is
      URL        : constant String := Data_Features & Integer'Image (Data_ID);
      URL_Object : AWS.URL.Object;
      Desc       :  Unbounded_String;
   begin
      --  URL.Parse parses an URL and returns an Object representing this URL.
      --  It is then possible to extract each part of the URL with other
      --  AWS.URL services.
      URL_Object := AWS.URL.Parse (URL);
      Assert (AWS.URL.Is_Valid (URL_Object),
              "Get_Data_Description_By_ID object returned by URL " &
                URL & "is invalid");
      return Desc;

   end Get_Data_Description_By_ID;

   --  ------------------------------------------------------------------------

   function Get_Data_Features (Data_ID : Integer) return Features_Map is
      URL      : constant String :=
                   Openml_Prefix & Data_Features & Integer'Image (Data_ID);
      Features : Features_Map;
      Data     : Json_Data;
   begin
      Data := Get_Json_Content_From_Openml_Api (URL);
      --        Features := GNATCOLL.JSON.

      return Features;

   end Get_Data_Features;

   --  ------------------------------------------------------------------------

   function Get_Data_Info_By_Name (Name : String; Version : Integer)
                                    return Json_Data is
      Data : Json_Data;
   begin
      return Data;

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
   function Get_Json_Content_From_Openml_Api (URL : String) return Json_Data is
      use AWS.Client;
      use AWS.Response;
      Data              : Json_Data;
      JSON_Message      : constant String := Message_Body (Get (URL));
      JSON_Main_Node    : JSON_Value := Create;
      JSON_Result_Array : JSON_Array := Empty_Array;
      Pair_Settings     : Pair_Settings_Vector;

      procedure Parse (Name : UTF8_String; Value : JSON_Value) is
      begin
            null;
      end Parse;

   begin
      JSON_Main_Node := Read (JSON_Message);
      JSON_Result_Array := Get (JSON_Main_Node);

      if Length (JSON_Result_Array) > 0 then
         for index in 1 .. Length (JSON_Result_Array) loop
            Pair_Settings.Append (Get (JSON_Result_Array, index));
         end loop;
      end if;
      Map_JSON_Object (JSON_Main_Node, Parse'Access);

      return Data;

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
