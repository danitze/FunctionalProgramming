module Main where

import Data.List (transpose)

data Neuron = Neuron {
    weight :: [Float],
    bias :: Float,
    activation :: Float,
    delta :: Float
} deriving (Show)

type Layer = [Neuron]
type Network = [Layer]

trainingData :: [([Float], [Float])]
trainingData = [ ([0, 0], [0])
               , ([0, 1], [1])
               , ([1, 0], [1])
               , ([1, 1], [0]) ]

testData :: [([Float], [Float])]
testData = trainingData

sigmoid :: Float -> Float
sigmoid x = 1 / (1 + exp (-x))

sigmoidPrime :: Float -> Float
sigmoidPrime x = s * (1 - s) where s = sigmoid x

forwardNeuron :: [Float] -> Neuron -> Neuron
forwardNeuron inputs neuron = neuron { activation = act }
    where act = sigmoid (sum (zipWith (*) inputs (weight neuron)) + bias neuron)

forwardLayer :: [Float] -> Layer -> [Float]
forwardLayer inputs layer = map (\neuron -> activation (forwardNeuron inputs neuron)) layer

forwardNetwork :: [Float] -> Network -> [Float]
forwardNetwork inputs network = foldl forwardLayer inputs network

-- Calculate gradients for the output layer
outputGradients :: [Float] -> Layer -> Layer
outputGradients expectedOutputs outputLayer = zipWith calcGradient outputLayer expectedOutputs
    where calcGradient neuron expected = neuron { delta = (activation neuron - expected) * sigmoidPrime (activation neuron) }

-- Backpropagation for one layer
backpropLayer :: Layer -> Layer -> Layer
backpropLayer nextLayer currentLayer = zipWith calcDelta currentLayer adjustedWeights
    where
        adjustedWeights = transpose $ map weight nextLayer
        calcDelta neuron weightsNext = neuron { delta = (sum $ zipWith (*) weightsNext (map delta nextLayer)) * sigmoidPrime (activation neuron) }

-- Update weights and biases for a single neuron
updateNeuron :: Float -> [Float] -> Neuron -> Neuron
updateNeuron learningRate inputs neuron = neuron { weight = newWeights, bias = newBias }
    where
        newWeights = zipWith (\w i -> w - learningRate * delta neuron * i) (weight neuron) inputs
        newBias = bias neuron - learningRate * delta neuron

-- Update weights and biases for a single layer
updateLayer :: Float -> [Float] -> Layer -> Layer
updateLayer learningRate inputs layer = map (updateNeuron learningRate inputs) layer

-- Update the entire network
updateNetwork :: Float -> [Float] -> Network -> Network
updateNetwork learningRate inputs network = zipWith (updateLayer learningRate) (inputs : (map (map activation) network)) network

-- Train iteration
trainIteration :: Float -> [Float] -> [Float] -> Network -> Network
trainIteration learningRate inputs expectedOutputs network = updatedNetwork
    where
        output = forwardNetwork inputs network
        outputLayerGradients = outputGradients expectedOutputs (last network)
        backpropagatedLayers = tail $ scanr backpropLayer (outputLayerGradients) network -- Properly propagate back through all layers
        updatedNetwork = updateNetwork learningRate inputs backpropagatedLayers

-- Function to train the network over multiple epochs
trainNetwork :: Float    -- Learning rate
             -> Int      -- Number of epochs
             -> Network  -- Initial network
             -> [([Float], [Float])] -- Training data (input, expected output)
             -> Network  -- Trained network
trainNetwork learningRate epochs network trainingData = foldl trainEpoch network [1..epochs]
    where
        trainEpoch net _ = foldl (trainOnSample learningRate) net trainingData

-- Function to train the network on one sample
trainOnSample :: Float        -- Learning rate
              -> Network      -- Current state of the network
              -> ([Float], [Float]) -- One sample (input, expected output)
              -> Network      -- Updated network
trainOnSample learningRate network (input, expectedOutput) = trainIteration learningRate input expectedOutput network

-- Function to test the network
testNetwork :: Network -> [([Float], [Float])] -> Float
testNetwork network testData = meanSquaredError allOutputs allExpected
    where
        allOutputs = concatMap (\(input, _) -> forwardNetwork input network) testData
        allExpected = concatMap snd testData
        meanSquaredError outputs expecteds = sum (zipWith mse outputs expecteds) / fromIntegral (length outputs)
        mse output expected = (output - expected) ** 2

-- Define a network structure for XOR problem
createNetwork :: Network
createNetwork = [[Neuron [0.15, 0.20] 0.35 0 0, Neuron [0.25, 0.30] 0.35 0 0],  -- First hidden layer
                 [Neuron [0.40, 0.45] 0.60 0 0, Neuron [0.50, 0.55] 0.60 0 0]]  -- Output layer

-- Main function to create, train, and test the network
main :: IO ()
main = do
    let network = createNetwork
    let learningRate = 0.1
    let epochs = 1000

    let trainedNetwork = trainNetwork learningRate epochs network trainingData

    let testPerformance = testNetwork trainedNetwork testData
    putStrLn $ "Test Performance: " ++ show testPerformance
