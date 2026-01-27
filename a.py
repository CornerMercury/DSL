import torch
from torchvision import datasets, transforms
from torch.utils.data import DataLoader, Subset
import os

# --- IMPLEMENTATION OF DECISION #1: DATA AUGMENTATION ---

mean = torch.Tensor([0.485, 0.456, 0.406])
std = torch.Tensor([0.229, 0.224, 0.225])

# 1. Training Transform: Heavily augmented to prevent overfitting
# We use RandomResizedCrop (zoom), Flip, Rotation, and Color Jitter.
transform_train = transforms.Compose([
    transforms.RandomResizedCrop(224), # Crop to 224 (standard ResNet input)
    transforms.RandomHorizontalFlip(),
    transforms.RandomRotation(15),     # Rotate up to 15 degrees
    transforms.ColorJitter(brightness=0.1, contrast=0.1, saturation=0.1),
    transforms.ToTensor(),
    transforms.Normalize(mean.tolist(), std.tolist()),
])

# 2. Test/Val Transform: Deterministic (No Randomness)
# We just Resize and CenterCrop.
transform_test = transforms.Compose([
    transforms.Resize(256),
    transforms.CenterCrop(224), # Crop to 224
    transforms.ToTensor(),
    transforms.Normalize(mean.tolist(), std.tolist()),
])

train_path = content_path + '/food-101/train'
test_path = content_path + '/food-101/test'

# We load the training folder TWICE. 
# Once with augmentation (for the training split)
# Once without augmentation (for the validation split)
full_train_dataset = datasets.ImageFolder(train_path, transform=transform_train)
full_val_dataset = datasets.ImageFolder(train_path, transform=transform_test)

# Load test set (always no augmentation)
test_dataset = datasets.ImageFolder(test_path, transform=transform_test)

# Create train val split using Indices
# We generate random indices, then assign them to the appropriate dataset version
n = len(full_train_dataset)
n_val = int(n / 10)

# Get random indices
train_indices, val_indices = torch.utils.data.random_split(range(n), [n - n_val, n_val])

# Create the final Subsets
train_set = Subset(full_train_dataset, train_indices) # Uses Augmented transform
val_set = Subset(full_val_dataset, val_indices)       # Uses Clean transform

print(f"Train size: {len(train_set)}, Val size: {len(val_set)}, Test size: {len(test_dataset)}")

# The number of images to process in one go.
batch_size = 128 # If you get "CUDA out of memory", change this to 64 or 32

def get_num_workers():
    suggested_workers = 0
    if hasattr(os, 'sched_getaffinity'):
        try:
            suggested_workers = len(os.sched_getaffinity(0))
        except Exception:
            pass
    if suggested_workers == 0:
        cpu_count = os.cpu_count()
        if cpu_count is not None:
            suggested_workers = cpu_count
    num_workers = min(8, suggested_workers)
    return num_workers

num_workers = get_num_workers()
print(f"Using {num_workers} workers")

# Dataloaders
loader_train = DataLoader(train_set, batch_size=batch_size, shuffle=True, num_workers=num_workers)
loader_val = DataLoader(val_set, batch_size=batch_size, shuffle=False, num_workers=num_workers) # Shuffle=False for val is standard
loader_test = DataLoader(test_dataset, batch_size=batch_size, shuffle=False, num_workers=num_workers)