with Ada.Strings.Wide_Unbounded; use Ada.Strings.Wide_Unbounded;
with Ada.Containers.Vectors;

package Etsy is

   type Product is tagged private;
   type Products is private;

   procedure Init (P : in out Product);

   procedure Output (P : Product);

private

   type Images_Arr is array (1 .. 10) of Unbounded_Wide_String;

   type Variation is record
      Variation_Type : Unbounded_Wide_String;
      Name           : Unbounded_Wide_String;
      Values         : Unbounded_Wide_String;
   end record;

   type Product is tagged record
      Sku           : Unbounded_Wide_String := Null_Unbounded_Wide_String;
      Title         : Unbounded_Wide_String := Null_Unbounded_Wide_String;
      Description   : Unbounded_Wide_String := Null_Unbounded_Wide_String;
      Price         : Float;
      Currency_Code : String (1 .. 3);
      Quantity      : Integer;
      Tags          : Unbounded_Wide_String := Null_Unbounded_Wide_String;
      Materials     : Unbounded_Wide_String := Null_Unbounded_Wide_String;
      Images        : Images_Arr;
      Variation_1   : Variation;
      Variation_2   : Variation;
   end record;

   package Product_Vectors is
     new Ada.Containers.Vectors (Natural, Product);

   type Products is new Product_Vectors.Vector with null record;

end Etsy;
