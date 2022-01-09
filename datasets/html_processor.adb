--  From https://franckbehaghel.eu/programming/ada/ada-web-server-example/
--       http_req.php

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1;

with Stack;

package body Html_Processor is

   Address_Str    : constant Unbounded_String := To_Unbounded_String ("--address");
   Tag_Str        : constant Unbounded_String := To_Unbounded_String ("--tag");
   Comment_Str    : constant Unbounded_String := To_Unbounded_String ("--comment");
   Stack_Str      : constant Unbounded_String := To_Unbounded_String ("--stack");
   Script_Str    : constant Unbounded_String := To_Unbounded_String ("--script");
   Configuration : Config_Maps.Map := Config_Maps.Empty_Map;

   procedure Print_Element (S : Unbounded_String) is
   begin
      Put (Ada.Strings.Unbounded.To_String (S));
   end Print_Element;

   package Tag_Stack is new Stack (Size => 3000, Elem => Unbounded_String, Print => Print_Element);

   type Parsing_Context (Default_Val : Integer) is record
      Stack             : Tag_Stack.Stack;
      Is_Inside_Tag     : Boolean := False;
      Is_Inside_Script  : Boolean := False;
      Is_Inside_Comment : Boolean := False;
      Is_Endding_Tag    : Boolean := False;

      State              : Parser_State := Init;
      Last_Tag_Index     : Natural      := Default_Val;
      Last_Content_Index : Natural      := Default_Val;
      Tag_Name           : Unbounded_String;
   end record;

   function Min (A, B : Natural) return Natural is
   begin
      if (A < B) then
         return A;
      else
         return B;
      end if;
   end Min;

   function Is_Empty (S : String) return Boolean is
      Result : Boolean   := True;
      Char   : Character := ' ';
   begin
      for I in S'First .. S'Last loop
         Char := S (I);
         if Char /= ' ' and
            Char /= Ada.Characters.Latin_1.CR and
            Char /= Ada.Characters.Latin_1.LF and
            Char /= Ada.Characters.Latin_1.HT
         then
            Result := False;
            exit;
         end if;
      end loop;

      return Result;

   end Is_Empty;

   function Get_Tag_Name (S : String; I : Natural) return Unbounded_String is
      Tag_Name       : Unbounded_String;
      Index_Tag      : Natural := 0;
      Index_Tag_Sort : Natural := 0;
      Index_Tag_Full : Natural := 0;
   begin
      Index_Tag_Sort := Fixed.Index (S (I .. S'Last), ">");
      Index_Tag_Full := Fixed.Index (S (I .. S'Last), " ");

      if (Index_Tag_Sort = 0) then
         Index_Tag_Sort := S'Last;
      end if;
      if (Index_Tag_Full = 0) then
         Index_Tag_Full := S'Last;
      end if;

      Index_Tag := Min (Index_Tag_Sort, Index_Tag_Full) - 1;
      Tag_Name  := To_Unbounded_String (S (I + 1 .. Index_Tag));

      return Tag_Name;

   end Get_Tag_Name;

   function Get_Full_Tag_Name (S : String; I : Natural) return Unbounded_String is
      Full_Tag_Name : Unbounded_String;
      Index_Tag     : Natural := 0;
   begin
      Index_Tag := Fixed.Index (S (I .. S'Last), ">");
      Full_Tag_Name := To_Unbounded_String (S (I + 1 .. Index_Tag));

      return Full_Tag_Name;

   end Get_Full_Tag_Name;

   function Get_End_Tag_Name (Tag : Unbounded_String) return Unbounded_String is
   begin
      return Tail (Tag, Length (Tag) - 1);

   end Get_End_Tag_Name;

   function Is_Tag_Open (S : String; I : Natural; Tag : Unbounded_String)
                         return Boolean is
      use type Unbounded_String;
      Index_Tag   : Natural := Fixed.Index (S (I .. S'Last), ">");
      Is_Tag_Open : Boolean := S (Index_Tag - 1) /= '/';
   begin
      Is_Tag_Open := Is_Tag_Open and Head (Tag, 1) /= "!";
      Is_Tag_Open := Is_Tag_Open and Tag /= "br";
      Is_Tag_Open := Is_Tag_Open and Tag /= "hr";
      Is_Tag_Open := Is_Tag_Open and Tag /= "img";
      Is_Tag_Open := Is_Tag_Open and Tag /= "input";
      Is_Tag_Open := Is_Tag_Open and Tag /= "meta";
      Is_Tag_Open := Is_Tag_Open and Tag /= "link";
      return Is_Tag_Open;

   end Is_Tag_Open;

   function Is_End_Tag (Tag : Unbounded_String) return Boolean is
      use type Unbounded_String;
      Is_End_Tag : Boolean := Head (Tag, 1) = "/";
   begin
      return Is_End_Tag;

   end Is_End_Tag;

   function Is_Script_Tag (Tag : Unbounded_String) return Boolean is
      use type Unbounded_String;
      Is_Script_Tag : Boolean := Tag = "script" or Tag = "/script";
   begin
      return Is_Script_Tag;

   end Is_Script_Tag;

   function Is_Comment_Tag (Tag : Unbounded_String) return Boolean is
      use type Unbounded_String;
      Is_Comment_Tag : Boolean := Head (Tag, 3) = "!--";
   begin
      return Is_Comment_Tag;

   end Is_Comment_Tag;

   procedure Display_Content (S : in String; I : in Integer;
                              C : in out Parsing_Context) is
   begin
      if ((Configuration.Contains (Script_Str) and C.Is_Inside_Script) or  (not C.Is_Inside_Script)) then
         if (not C.Is_Inside_Comment and (I > 2))
           and then (not Is_Empty (S (C.Last_Content_Index .. I - 1)))
         then
            if (Configuration.Contains (Stack_Str)) then
               Tag_Stack.Print_All (C.Stack);
            end if;
            if (Configuration.Contains (Address_Str)) then
               Put (" [" & C.Last_Content_Index'Img & " .." & Natural'Image (I) & " ]");
            end if;
            Put_Line (S (C.Last_Content_Index .. I - 1));
         end if;
      end if;
   end Display_Content;

   procedure Display_Tag (S : in String; I : in Integer;
                          C : in out Parsing_Context) is
   begin
      if ((Configuration.Contains (Tag_Str) and not C.Is_Inside_Comment) or
          (Configuration.Contains (Comment_Str) and C.Is_Inside_Comment))
      then
         if ((I > 2)) and then (not Is_Empty (S (C.Last_Tag_Index .. I - 1))) then
            if (C.Is_Inside_Comment) then
               Put ("Comment ");
            else
               Put ("Tag ");
            end if;
            if (Configuration.Contains (Address_Str)) then
               Put (" [" & C.Last_Tag_Index'Img & " .." & Natural'Image (I) & " ]");
            end if;
            Put_Line (S (C.Last_Tag_Index .. I - 1));
         end if;
      end if;

   end Display_Tag;

   procedure Process_Request_Script (S : in String; I : in Integer;
                                     C : in out Parsing_Context) is
      End_Tag_Name : Unbounded_String;
      use type Unbounded_String;
   begin
      if (Is_Script_Tag (C.Tag_Name) and C.Is_Endding_Tag) then
         End_Tag_Name := Get_End_Tag_Name (C.Tag_Name);
         Tag_Stack.Pop_Until (C.Stack, End_Tag_Name);
         Display_Content (S, I, C);
         C.Last_Tag_Index   := I + 1;
         C.Is_Inside_Script := False;
         C.Is_Inside_Tag    := True;
      end if;
   end Process_Request_Script;

   procedure Process_Request_All_Tag (S : in String; I : in Integer;
                                      C : in out Parsing_Context) is
      End_Tag_Name : Unbounded_String;
      use type Unbounded_String;
   begin
      C.Is_Inside_Script := Is_Script_Tag (C.Tag_Name) and not C.Is_Endding_Tag;
      if (C.Is_Endding_Tag) then

         End_Tag_Name := Get_End_Tag_Name (C.Tag_Name);

         Tag_Stack.Pop_Until (C.Stack, End_Tag_Name);
      else
         if (Is_Tag_Open (S, I, C.Tag_Name)) then
            Tag_Stack.Push (C.Stack, C.Tag_Name);
         end if;
      end if;
      C.Is_Inside_Tag := True;

   end Process_Request_All_Tag;

   procedure Process_Request_Internal (S : String) is
      Char : Character := ' ';
      C    : Parsing_Context (S'First);
      use type Unbounded_String;
   begin
      for I in S'Range loop
         Char := S (I);

         -- Update Comment meta info
         if (Char = '>' and C.Is_Inside_Comment and I > 2)
           and then (S (I - 1) = '-' and S (I - 2) = '-')
         then
            Display_Tag (S, I, C);
            C.Is_Inside_Comment  := False;
            C.Last_Content_Index := I + 1;
         end if;

         if (Char = '<') and not C.Is_Inside_Comment then
            C.Tag_Name          := Get_Tag_Name (S, I);
            C.Is_Inside_Comment := Is_Comment_Tag (C.Tag_Name);
            if (C.Is_Inside_Comment) then
               C.Last_Tag_Index := I + 1;
               Display_Content (S, I, C);
            end if;
         end if;

         -- Update Html meta info
         if (Char = '>' and C.Is_Inside_Tag) then
            Display_Tag (S, I, C);
            C.Is_Inside_Tag      := False;
            C.Last_Content_Index := I + 1;
         end if;

         if (Char = '<') and not C.Is_Inside_Comment then

            C.Tag_Name       := Get_Tag_Name (S, I);
            C.Is_Endding_Tag := Is_End_Tag (C.Tag_Name);

            if (C.Is_Inside_Script) then
               Process_Request_Script (S, I, C);
            else
               Display_Content (S, I, C);
               C.Last_Tag_Index := I + 1;
               Process_Request_All_Tag (S, I, C);
            end if;

         end if;

      end loop;

      Put_Line ("Result :");
      Tag_Stack.Print_All (C.Stack);

   end Process_Request_Internal;

   procedure Process_Request (Request : String) is
   begin
      Put_Line ("Process_Request");
      Process_Request_Internal (Request);

   end Process_Request;

   procedure Process_Request (Request : Unbounded_String) is
   begin
      Put_Line ("Process_Request");
      Process_Request_Internal (To_String (Request));

   end Process_Request;

   procedure Process_Request (Config : Config_Maps.Map; Request : String) is
   begin
      Put_Line ("Process_Request");
      Configuration := Config;
      Process_Request_Internal (Request);

   end Process_Request;

   procedure Process_Request
     (Config : Config_Maps.Map; Request : Unbounded_String) is
   begin
      Put_Line ("Process_Request");
      Configuration := Config;
      Process_Request_Internal (To_String (Request));

   end Process_Request;

end Html_Processor;
