class Animal():
    def __init__(self, species, carnivore):
        self.species = species
        self.carnivore = carnivore

    def eat(self):
        if self.carnivore:
            print("Eating meat")
        else:
            print("Eating vegetables")

    def jump(self):
        print(f"{self.species} is jumping")


rabbit = Animal("rabbit", False)
wolf = Animal("wolf", True)
animaliList = [rabbit, wolf]
#Polymorphism
for ob in animaliList:
    ob.eat()
    ob.jump()


