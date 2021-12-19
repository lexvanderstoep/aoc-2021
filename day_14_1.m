#import <Foundation/Foundation.h>

NSMutableDictionary<NSString*, NSNumber*>* parsePolymerTemplate(NSString* filename, NSString** firstPair, NSString** lastPair) {
    NSString* content = [NSString stringWithContentsOfFile:filename
                                              encoding:NSUTF8StringEncoding
                                                 error:NULL];
    NSArray* lines = [content componentsSeparatedByString:@"\n"];
    NSString* templatePolymer = [lines objectAtIndex:0];
    NSMutableDictionary<NSString*, NSNumber*>* dictionary = [[NSMutableDictionary alloc] init];

    for (int i = 0; i < [templatePolymer length] - 1; i++) {
        NSString* pair = [templatePolymer substringWithRange:NSMakeRange(i, 2)];
        if (![dictionary objectForKey:pair]) {
            dictionary[pair] = 0;
        }
        NSNumber* count = [dictionary objectForKey:pair];
        dictionary[pair] = [NSNumber numberWithInt:([count intValue] + [@1 intValue])];
    }

    *firstPair = [templatePolymer substringWithRange:NSMakeRange(0, 2)];
    *lastPair = [templatePolymer substringWithRange:NSMakeRange([templatePolymer length] - 2, 2)];

    return dictionary;
}

NSMutableDictionary<NSString*, NSString*>* parseInsertionRules(NSString* filename) {
    NSString* content = [NSString stringWithContentsOfFile:filename
                                              encoding:NSUTF8StringEncoding
                                                 error:NULL];
    NSArray* lines = [content componentsSeparatedByString:@"\n"];
    NSMutableDictionary<NSString*, NSString*>* rules = [[NSMutableDictionary alloc] init];

    for (int i = 2; i < [lines count]; i++) {
        NSString* line = [lines objectAtIndex:i];
        NSString* pair = [line substringWithRange:NSMakeRange(0, 2)];
        NSString* insertionChar = [line substringWithRange:NSMakeRange(6, 1)];
        rules[pair] = insertionChar;
    }

    return rules;
}

NSMutableDictionary<NSString*, NSNumber*>* applyStep(NSMutableDictionary<NSString*, NSNumber*>* polymer, NSString** firstPair, NSString** lastPair, NSMutableDictionary<NSString*, NSString*>* insertionRules) {
    NSMutableDictionary<NSString*, NSNumber*>* newPolymer = [[NSMutableDictionary alloc] init];
    for(id pair in polymer) {
        NSNumber* count = [polymer objectForKey:pair];
        NSString* insertionCharacter = [insertionRules objectForKey:pair];
        NSString* newFirstPair = [[pair substringWithRange:NSMakeRange(0,1)] stringByAppendingString: insertionCharacter];
        NSString* newSecondPair = [insertionCharacter stringByAppendingString: [pair substringWithRange:NSMakeRange(1,1)]];
        if (![newPolymer objectForKey:newFirstPair]) {
            newPolymer[newFirstPair] = 0;
        }
        if (![newPolymer objectForKey:newSecondPair]) {
            newPolymer[newSecondPair] = 0;
        }
        newPolymer[newFirstPair] = [NSNumber numberWithInt:([newPolymer[newFirstPair] intValue] + [count intValue])];
        newPolymer[newSecondPair] = [NSNumber numberWithInt:([newPolymer[newSecondPair] intValue] + [count intValue])];
    }

    NSString* firstPairInsertionCharacter = [insertionRules objectForKey:*firstPair];
    NSString* lastPairInsertionCharacter = [insertionRules objectForKey:*lastPair];
    *firstPair = [[*firstPair substringWithRange:NSMakeRange(0,1)] stringByAppendingString: firstPairInsertionCharacter];
    *lastPair = [lastPairInsertionCharacter stringByAppendingString: [*lastPair substringWithRange:NSMakeRange(1,1)]];

    return newPolymer;
}

NSMutableDictionary<NSString*, NSNumber*>* countOccurences(NSMutableDictionary<NSString*, NSNumber*>* polymer, NSString* firstPair, NSString* lastPair) {
    NSMutableDictionary<NSString*, NSNumber*>* occurences = [[NSMutableDictionary alloc] init];
    for (id pair in polymer) {
        NSNumber* count = polymer[pair];
        NSString* firstCharacter = [pair substringWithRange:NSMakeRange(0,1)];
        NSString* secondCharacter = [pair substringWithRange:NSMakeRange(1,1)];
        if (![occurences objectForKey:firstCharacter]) {
            occurences[firstCharacter] = 0;
        }
        if (![occurences objectForKey:secondCharacter]) {
            occurences[secondCharacter] = 0;
        }
        occurences[firstCharacter] = [NSNumber numberWithInt:([occurences[firstCharacter] intValue] + [count intValue])];
        occurences[secondCharacter] = [NSNumber numberWithInt:([occurences[secondCharacter] intValue] + [count intValue])];
    }
    NSString* firstCharacter = [firstPair substringWithRange:NSMakeRange(0,1)];
    NSString* lastCharacter = [lastPair substringWithRange:NSMakeRange(1,1)];
    occurences[firstCharacter] = [NSNumber numberWithInt: ([occurences[firstCharacter] intValue] + 1)];
    occurences[lastCharacter] = [NSNumber numberWithInt: ([occurences[lastCharacter] intValue] + 1)];

    NSMutableDictionary<NSString*, NSNumber*>* singleOccurences = [[NSMutableDictionary alloc] init];
    for (id character in occurences) {
        NSNumber* count = occurences[character];
        singleOccurences[character] = [NSNumber numberWithInt: [occurences[character] intValue] / 2];
    }
    
    return singleOccurences;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSLog(@"Hello, World!");
        NSString* firstPair;
        NSString* lastPair;
        id polymer = parsePolymerTemplate(@"input/input_14_1.txt", &firstPair, &lastPair);
        id insertionRules = parseInsertionRules(@"input/input_14_1.txt");

        int numberOfSteps = 10;

        for (int step = 0; step < numberOfSteps; step++) {
            polymer = applyStep(polymer, &firstPair, &lastPair, insertionRules);
        }

        NSLog(@"Occurences: %@", countOccurences(polymer, firstPair, lastPair));
    }
    return 0;
}
