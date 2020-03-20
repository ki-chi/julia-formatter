struct Animal
    name::String
end

sayname(a::Animal) = print("My name is $(a.name).")

mydog = Animal("Tom")

sayname(mydog)
