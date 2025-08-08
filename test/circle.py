# circle.py

# This script demonstrates basic Python syntax for testing syntax highlighting.
# It includes functions, a class, control flow, and various comment styles.

import math  # Importing the math module for mathematical operations


'''
This is a block-style comment using triple single quotes.
It's not ideal for real code, but useful for testing syntax highlighting.
'''


"""
This is a block-style comment using triple double quotes.
It's not ideal for real code, but useful for testing syntax highlighting.
"""



# Single-line strings
greeting1 = "Hello"
greeting2 = 'Hi there'

# Multiline strings, should be rendered as strings not commnets
multiline1 = """This is
a multiline string
using triple double quotes."""

multiline2 = '''This is
also a multiline string
using triple single quotes.'''

# Docstring example
def say_hello():
    """This function prints a greeting."""
    print("Hello!")


def greet(name):
    """Greets the user by name using f-string formatting."""
    print(f"Hello, {name}!")


#  a docstring appears straight after a function of class definition
#  these are effectively multi-line comments
def factorial(n):

    """
    Calculates the factorial of a non-negative integer n.
    
    Parameters:
        n (int): The number to compute the factorial of.
    
    Returns:
        int or None: The factorial of n, or None if n is negative.
    """
    if n < 0:
        return None  # Factorial is undefined for negative numbers
    result = 1
    for i in range(2, n + 1):
        result *= i  # Multiply result by each number from 2 to n
    return result

class Circle:
    """Represents a circle and provides methods to compute its area."""

    def __init__(self, radius):
        self.radius = radius  # Store the radius

    def area(self):
        """Returns the area of the circle."""
        return math.pi * self.radius ** 2

    def __str__(self):
        """Returns a string representation of the circle."""
        return f"Circle with radius {self.radius}"

# Multiline string assigned to a variable
long_description = """This is a multiline string.
It spans several lines and is stored in a variable.
Useful for long text blocks or embedded documentation."""

if __name__ == "__main__":
    # Main execution block
    greet("World")  # Call the greet function
    say_hello()
    print("Factorial of 5:", factorial(5))  # Display factorial of 5
    c = Circle(3)  # Create a Circle object with radius 3
    print(c)  # Print the Circle object
    print("Area:", c.area())  # Print the area of the circle
    print(long_description)  # Print the multiline string
