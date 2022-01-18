
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Gnat.Regpat;

package Regexep is
   use GNAT.Regpat;

   package Matches_Package is new Ada.Containers.Vectors
     (Natural, Match_Location);
   subtype Matches_List is Matches_Package.Vector;

   package Match_Strings_Package is new Ada.Containers.Vectors
     (Natural, Unbounded_String);
   subtype Match_Strings_List is Match_Strings_Package.Vector;

   function Find_Match
     (Compiled_Expression : Gnat.Regpat.Pattern_Matcher; Text : String;
      First, Last         : out Positive; Found : out Boolean;
      Group_Index         : Natural := 0) return Matches_List;
   function Get_Groups (Matches : Matches_List) return Matches_List;
   function Get_Groups (Matches : Match_Strings_List) return Match_Strings_List;

end Regexep;
