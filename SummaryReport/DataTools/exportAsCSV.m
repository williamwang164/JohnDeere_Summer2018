clc;clear;
data = dir('*.mat');
for k = 1: length(data)
    file = data(k);
    [filepath, name, ext] = fileparts(file.name);
    tocsv(name);
end

function tocsv(n)
    load(n);
    clear DA feedforwardFlag logFileName modelUsed predictionFileUsed simulationParameters
    varNames = {'biomassAtVehiclePosition';'enginePower';'groundSpeed';'latitudeVectorDegreesSimulationTimeBased';'longitudeVectorDegreesSimulationTimeBased';'predictedBiomass';'rotorPressure';'targetEnginePower';'targetRotorPressure';'traveledDistance'; 'propulsionPower'; 'fuelRate_LiterPerHour'};
    x(:,1) = biomassAtVehiclePosition.Time;
    for i = 1:length(varNames)
        x(:,i+1) = eval([varNames{i} '.Data;']);
    end
    dlmwrite([n '.csv'],x, 'precision','%20.10f');
end




















