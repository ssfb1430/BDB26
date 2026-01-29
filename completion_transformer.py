"""
Completion Probability Prediction Model

This module defines a neural network for predicting completion probability
given WR features, DB features, and play context.
"""

from typing import Any

import torch
from lightning import LightningModule
from torch import Tensor, nn
from torch.optim import AdamW
from torchmetrics.functional import auroc

WRDIM, DBDIM, PLAYDIM = 2, 2, 12 # Example feature dimensions for WR, DB, and play context

class CompletionProbabilityModel(nn.Module):
    """
    Model for predicting completion probability from WR-DB matchup and play context.
    """

    def __init__(
        self,
        wr_features: int = 2,
        db_features: int = 2,
        play_features: int = 10,
        model_dim: int = 64,
        num_layers: int = 3,
        dropout: float = 0.2,
    ):
        """
        Initialize the CompletionProbabilityModel.

        Args:
            wr_features (int): Number of WR features (default: 2)
            db_features (int): Number of DB features (default: 2)
            play_features (int): Number of play context features (default: 10)
            model_dim (int): Dimension of the model's internal representations
            num_layers (int): Number of transformer encoder layers
            dropout (float): Dropout rate for regularization
        """
        super().__init__()
        
        self.wr_features = wr_features
        self.db_features = db_features
        self.play_features = play_features
        
        self.hyperparams = {
            "model_dim": model_dim,
            "num_layers": num_layers,
            "wr_features": wr_features,
            "db_features": db_features,
            "play_features": play_features,
        }

        # Separate embedding layers for each input type
        self.wr_embedding = nn.Sequential(
            nn.Linear(wr_features, model_dim),
            nn.ReLU(),
            nn.LayerNorm(model_dim),
            nn.Dropout(dropout),
        )
        
        self.db_embedding = nn.Sequential(
            nn.Linear(db_features, model_dim),
            nn.ReLU(),
            nn.LayerNorm(model_dim),
            nn.Dropout(dropout),
        )
        
        self.play_embedding = nn.Sequential(
            nn.Linear(play_features, model_dim),
            nn.ReLU(),
            nn.LayerNorm(model_dim),
            nn.Dropout(dropout),
        )

        # Transformer to process the sequence [WR, DB, play]
        dim_feedforward = model_dim * 4
        num_heads = max(2, model_dim // 32)  # Ensure even number of heads
        
        self.transformer_encoder = nn.TransformerEncoder(
            nn.TransformerEncoderLayer(
                d_model=model_dim,
                nhead=num_heads,
                dim_feedforward=dim_feedforward,
                dropout=dropout,
                batch_first=True,
            ),
            num_layers=num_layers,
        )

        # Decoder for binary classification (completion probability)
        self.decoder = nn.Sequential(
            nn.Linear(model_dim * 3, model_dim),  # 3 tokens: WR, DB, play
            nn.ReLU(),
            nn.Dropout(dropout),
            nn.Linear(model_dim, model_dim // 2),
            nn.ReLU(),
            nn.LayerNorm(model_dim // 2),
            nn.Linear(model_dim // 2, 1),  # Single output for probability
        )

    def forward(self, wr: Tensor, db: Tensor, play: Tensor) -> Tensor:
        """
        Forward pass of the model.

        Args:
            wr (Tensor): WR features of shape [batch_size, wr_features]
            db (Tensor): DB features of shape [batch_size, db_features]
            play (Tensor): Play context features of shape [batch_size, play_features]

        Returns:
            Tensor: Completion probability logits of shape [batch_size, 1]
        """
        batch_size = wr.size(0)
        
        # Embed each input type
        wr_embed = self.wr_embedding(wr).unsqueeze(1)      # [B, 1, model_dim]
        db_embed = self.db_embedding(db).unsqueeze(1)      # [B, 1, model_dim]
        play_embed = self.play_embedding(play).unsqueeze(1)  # [B, 1, model_dim]
        
        # Concatenate into sequence [WR, DB, play]
        x = torch.cat([wr_embed, db_embed, play_embed], dim=1)  # [B, 3, model_dim]
        
        # Apply transformer
        x = self.transformer_encoder(x)  # [B, 3, model_dim]
        
        # Flatten the sequence dimension
        x = x.reshape(batch_size, -1)  # [B, 3 * model_dim]
        
        # Decode to probability
        x = self.decoder(x)  # [B, 1]
        
        return x


class LitCompletionModel(LightningModule):
    """
    Lightning module for training completion probability prediction.
    """

    def __init__(
        self,
        wr_features: int = WRDIM,
        db_features: int = DBDIM,
        play_features: int = PLAYDIM,
        model_dim: int = 64,
        num_layers: int = 3,
        dropout: float = 0.2,
        learning_rate: float = 1e-3,
    ):
        """
        Initialize the LitCompletionModel.

        Args:
            wr_features (int): Number of WR features
            db_features (int): Number of DB features
            play_features (int): Number of play context features
            model_dim (int): Dimension of model
            num_layers (int): Number of transformer layers
            dropout (float): Dropout rate
            learning_rate (float): Learning rate
        """
        super().__init__()
        
        self.model = CompletionProbabilityModel(
            wr_features=wr_features,
            db_features=db_features,
            play_features=play_features,
            model_dim=model_dim,
            num_layers=num_layers,
            dropout=dropout,
        )
        
        self.learning_rate = learning_rate
        self.num_params = sum(p.numel() for p in self.model.parameters() if p.requires_grad)
        self.hparams["params"] = self.num_params
        
        for k, v in self.model.hyperparams.items():
            self.hparams[k] = v
        
        self.save_hyperparameters()
        
        # Binary cross-entropy loss for probability prediction
        self.loss_fn = nn.BCEWithLogitsLoss()

    def forward(self, wr: Tensor, db: Tensor, play: Tensor) -> Tensor:
        """Forward pass."""
        return self.model(wr, db, play)

    def training_step(self, batch: tuple[Tensor, Tensor, Tensor, Tensor], batch_idx: int) -> Tensor:
        """Training step."""
        wr, db, play, y = batch
        
        y_hat = self.model(wr, db, play).squeeze(-1)
        loss = self.loss_fn(y_hat, y.float())
        
        # Calculate accuracy
        preds = (torch.sigmoid(y_hat) > 0.5).float()
        acc = (preds == y).float().mean()
        
        # Calculate AUC
        auc = auroc(torch.sigmoid(y_hat), y.long(), task='binary')
        
        self.log("train_loss", loss, on_step=False, on_epoch=True, prog_bar=True)
        self.log("train_acc", acc, on_step=False, on_epoch=True, prog_bar=True)
        self.log("train_auc", auc, on_step=False, on_epoch=True, prog_bar=True)
        return loss

    def validation_step(self, batch: tuple[Tensor, Tensor, Tensor, Tensor], batch_idx: int) -> Tensor:
        """Validation step."""
        wr, db, play, y = batch
        
        y_hat = self.model(wr, db, play).squeeze(-1)
        loss = self.loss_fn(y_hat, y.float())
        
        preds = (torch.sigmoid(y_hat) > 0.5).float()
        acc = (preds == y).float().mean()
        
        # Calculate AUC
        auc = auroc(torch.sigmoid(y_hat), y.long(), task='binary')
        
        self.log("val_loss", loss, on_step=False, on_epoch=True, prog_bar=True)
        self.log("val_acc", acc, on_step=False, on_epoch=True, prog_bar=True)
        self.log("val_auc", auc, on_step=False, on_epoch=True, prog_bar=True)
        return loss

    def predict_step(self, batch: tuple[Tensor, Tensor, Tensor, Tensor], batch_idx: int) -> Tensor:
        """Prediction step - returns probabilities."""
        wr, db, play, y = batch
        
        y_hat = self.model(wr, db, play).squeeze(-1)
        return torch.sigmoid(y_hat)  # Return probabilities

    def configure_optimizers(self) -> AdamW:
        """Configure optimizer."""
        return AdamW(self.parameters(), lr=self.learning_rate)


# Example usage
if __name__ == "__main__":
    # Create model
    model = LitCompletionModel(
        wr_features=2,
        db_features=2,
        play_features=10,
        model_dim=64,
        num_layers=3,
    )
    
    # Example batch
    batch = {
        'wr': torch.randn(32, 2),      # 32 samples, 2 WR features
        'db': torch.randn(32, 2),      # 32 samples, 2 DB features
        'play': torch.randn(32, 10),   # 32 samples, 10 play features
        'target': torch.randint(0, 2, (32,))  # Binary labels
    }
    
    # Forward pass
    output = model(batch['wr'], batch['db'], batch['play'])
    print(f"Output shape: {output.shape}")  # Should be [32, 1]
    
    # Get probabilities
    probs = torch.sigmoid(output)
    print(f"Probability range: [{probs.min():.3f}, {probs.max():.3f}]")