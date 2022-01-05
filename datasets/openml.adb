--  Based on scikit-learn/sklearn/datasets _openml.py

with Ada.Characters.Handling;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

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

   function Get_Data_Info_By_Name (Name : String; Version : Integer)
                                   return Json_Data;

   --  ------------------------------------------------------------------------

   procedure Fetch_Openml (Dataset_Name : String; Version : Integer;
                           Data_Id : in out Integer;
                           Return_X_Y : Boolean := False) is
      use Ada.Characters.Handling;
      Name_LC : constant String := To_Lower (Dataset_Name);
      Data_Info  : Json_Data;
      Data       : AWS.Response.Data;
      URL_Object : AWS.URL.Object;
   begin
      Data_Info := Get_Data_Info_By_Name (Dataset_Name, Version);
      Data_Id := Data_Info.ID;
      Data := AWS.Client.Get
          (URL => "http://perso.wanadoo.fr/pascal.obry/contrib.html");
   end ;

   --  ------------------------------------------------------------------------

   function Get_Data_Info_By_Name (Name : String; Version : Integer)
                                   return Json_Data is
      Data : Json_Data;
   begin
      return Data;

   end Get_Data_Info_By_Name;

   --  ------------------------------------------------------------------------

end Openml;
