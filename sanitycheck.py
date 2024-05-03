import torch
import torch.nn as nn
import torch.optim as optim
import numpy as np
import pandas as pd


# Define the neural network architecture
class NeuralNetwork(nn.Module):
    def __init__(self):
        super(NeuralNetwork, self).__init__()
        self.fc1 = nn.Linear(8, 16)  # 8 input neurons, 16 neurons in hidden layer
        self.fc2 = nn.Linear(16, 1)  # 16 neurons in hidden layer, 1 output neuron

    def forward(self, x):
        x = torch.relu(self.fc1(x))
        x = self.fc2(x)
        return x


# Dummy data for training and testing

all_data = pd.read_csv("eScooterDemand.csv")
all_data.head()


## plot average count over time to look for patterns that can be used to predict future demand
all_data["Date"] = pd.to_datetime(all_data["Date"], format="%d/%m/%Y")
all_data["Month"] = all_data["Date"].dt.month
all_data["Year"] = all_data["Date"].dt.year
all_data["Day"] = all_data["Date"].dt.day

## Show all plots on one figure
fig, ax = plt.subplots(2, 2, figsize=(8, 8))
sns.histplot(all_data["Count"], kde=True, ax=ax[0, 0]).set_title("Count Distribution")
all_data.groupby("Day").aggregate({"Count": "mean"}).plot(
    kind="bar", ax=ax[0, 1], title="Average Count by Day of Month"
)
all_data.groupby("Month").aggregate({"Count": "mean"}).plot(
    kind="bar", ax=ax[1, 0], title="Average Count by Month"
)
all_data.groupby("Year").aggregate({"Count": "mean"}).plot(
    kind="bar", ax=ax[1, 1], title="Average Count by Year"
)

plt.show()
X_train = torch.randn(1000, 8)  # 1000 samples, 8 features
y_train = torch.randn(1000, 1)  # 1000 target values
X_test = torch.randn(200, 8)  # 200 samples for testing


# Initialize the neural network
model = NeuralNetwork()

# Define loss function and optimizer
criterion = nn.MSELoss()
optimizer = optim.SGD(model.parameters(), lr=0.01)

# Training the neural network
epochs = 1000
for epoch in range(epochs):
    # Forward pass
    outputs = model(X_train)
    loss = criterion(outputs, y_train)

    # Backward pass and optimization
    optimizer.zero_grad()
    loss.backward()
    optimizer.step()

    if (epoch + 1) % 100 == 0:
        print(f"Epoch [{epoch+1}/{epochs}], Loss: {loss.item():.4f}")

# Testing the neural network
with torch.no_grad():
    predicted = model(X_test)

print("Predicted values:", predicted)
