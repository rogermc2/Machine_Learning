
with Gnat.Regpat;

package Regex is

   procedure Find_Match
     (Compiled_Expression : Gnat.Regpat.Pattern_Matcher; Text : String;
      First, Last         : out Positive; Found : out Boolean);
   function Get_Groups (Regex : String) return String;

end Regex;
