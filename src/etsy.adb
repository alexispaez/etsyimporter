with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Strings.Fixed;          use Ada.Strings.Fixed;
with Ada.Characters.Conversions; use Ada.Characters.Conversions;

package body Etsy is

   procedure Init (P : in out Product) is
   begin
      P :=
        Product'
          (Price         => 0.0,
           Currency_Code => "   ",
           Quantity      => 0,
           Images        => (others => Null_Unbounded_Wide_String),
           Variation_1   => (others => Null_Unbounded_Wide_String),
           Variation_2   => (others => Null_Unbounded_Wide_String),
           others        => Null_Unbounded_Wide_String);
   end Init;

   procedure Output (P : Product) is
      Separator : constant String := 50 * "-";
   begin
      Put_Line (Separator);
      Put_Line ("Product code: "  & To_String (To_Wide_String (P.Sku)));
      Put_Line ("Title: "         & To_String (To_Wide_String (P.Title)));
      Put_Line ("Description: "   & To_String
                (To_Wide_String (P.Description)));
      Put_Line ("Price: "         & Float'Image (P.Price));
      Put_Line ("Currency code: " & P.Currency_Code);
      Put_Line ("Quantity: "      & Integer'Image (P.Quantity));
      Put_Line ("Tags: "          & To_String (To_Wide_String (P.Tags)));
      Put_Line ("Materials: "     & To_String (To_Wide_String (P.Materials)));
      for I in P.Images'Range loop
         if P.Images (I) /= Null_Unbounded_Wide_String then
            Put_Line ("Image: "
                      & Integer'Image (I)
                      & " "
                      & To_String (To_Wide_String (P.Images (I))));
         end if;
      end loop;
   end Output;

end Etsy;
